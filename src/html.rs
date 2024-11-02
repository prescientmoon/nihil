use std::collections::HashMap;

use anyhow::anyhow;
use chrono::DateTime;
use chrono::TimeZone;
use jotdown::Alignment;
use jotdown::Container;
use jotdown::Event;
use jotdown::LinkType;
use jotdown::ListKind;
use jotdown::OrderedListNumbering::*;
use jotdown::SpanLinkType;

use crate::metadata::PageMetadata;
use crate::metadata::PageRoute;
use crate::template;
use crate::template::TemplateRenderer;

// {{{ Renderer
/// Render djot content as HTML.
pub fn render_html<'s>(
	metadata: &'s PageMetadata,
	mut events: impl Iterator<Item = Event<'s>>,
	mut out: impl std::fmt::Write,
) -> anyhow::Result<()> {
	let mut w = Writer::new(Some(metadata));
	events.try_for_each(|e| w.render_event(&e, &mut out))?;
	w.render_epilogue(&mut out)?;

	Ok(())
}
// }}}

pub struct Writer<'s> {
	list_tightness: Vec<bool>,
	states: Vec<State<'s>>,
	footnotes: Footnotes<'s>,
	metadata: Option<&'s PageMetadata>,
}

#[derive(Debug, Clone)]
enum State<'s> {
	TextOnly,
	Ignore,
	Raw,
	Math(bool),
	Aside(TemplateRenderer<'s>),
	Article(TemplateRenderer<'s>),
}

impl<'s> Writer<'s> {
	pub fn new(metadata: Option<&'s PageMetadata>) -> Self {
		Self {
			list_tightness: Vec::new(),
			states: Vec::new(),
			footnotes: Footnotes::default(),
			metadata,
		}
	}

	#[allow(clippy::single_match)]
	pub fn render_event(
		&mut self,
		e: &Event<'s>,
		mut out: impl std::fmt::Write,
	) -> anyhow::Result<()> {
		// {{{ Handle footnotes
		if let Event::Start(Container::Footnote { label }, ..) = e {
			self.footnotes.start(label, Vec::new());
			return Ok(());
		} else if let Some(events) = self.footnotes.current() {
			if matches!(e, Event::End(Container::Footnote { .. })) {
				self.footnotes.end();
			} else {
				events.push(e.clone());
			}
			return Ok(());
		}
		// }}}
		// {{{ Handle blocks which trigger the `Ignore` state.
		match e {
			Event::Start(Container::LinkDefinition { .. }, ..) => {
				self.states.push(State::Ignore);
				return Ok(());
			}
			Event::End(Container::LinkDefinition { .. }) => {
				assert!(matches!(self.states.last(), Some(State::Ignore)));
				self.states.pop();
			}

			Event::Start(Container::RawBlock { format } | Container::RawInline { format }, ..) => {
				if format == &"html" {
					self.states.push(State::Raw);
				} else {
					self.states.push(State::Ignore);
				};

				return Ok(());
			}
			Event::End(Container::RawBlock { format } | Container::RawInline { format }) => {
				if format == &"html" {
					assert!(matches!(self.states.last(), Some(State::Raw)));
				} else {
					assert!(matches!(self.states.last(), Some(State::Ignore)));
				};

				self.states.pop();
			}

			_ => {}
		}
		// }}}

		if matches!(self.states.last(), Some(State::Ignore)) {
			return Ok(());
		}

		match e {
			// {{{ Container start
			Event::Start(c, attrs) => {
				if matches!(self.states.last(), Some(&State::TextOnly)) {
					return Ok(());
				}

				match &c {
					Container::RawBlock { .. } => {}
					Container::RawInline { .. } => unreachable!(),
					Container::Footnote { .. } => unreachable!(),
					// {{{ List
					Container::List { kind, tight } => {
						self.list_tightness.push(*tight);
						match kind {
							ListKind::Unordered(..) | ListKind::Task(..) => out.write_str("<ul")?,
							ListKind::Ordered {
								numbering, start, ..
							} => {
								out.write_str("<ol")?;
								if *start > 1 {
									write!(out, r#" start="{}""#, start)?;
								}

								if let Some(ty) = match numbering {
									Decimal => None,
									AlphaLower => Some('a'),
									AlphaUpper => Some('A'),
									RomanLower => Some('i'),
									RomanUpper => Some('I'),
								} {
									write!(out, r#" type="{}""#, ty)?;
								}
							}
						}
					}
					// }}}
					// {{{ Link
					Container::Link(dst, ty) => {
						if matches!(ty, LinkType::Span(SpanLinkType::Unresolved)) {
							out.write_str("<a")?;
						} else {
							out.write_str(r#"<a href=""#)?;
							if matches!(ty, LinkType::Email) {
								out.write_str("mailto:")?;
							}
							write_attr(dst, &mut out)?;
							out.write_char('"')?;
						}
					}
					// }}}
					// {{{ Paragraph
					Container::Paragraph => {
						if self.list_tightness.last() == Some(&true) {
							return Ok(());
						}

						out.write_str("<p")?;
					}
					// }}}
					Container::Blockquote => out.write_str("<blockquote")?,
					Container::ListItem { .. } => out.write_str("<li")?,
					Container::TaskListItem { .. } => out.write_str("<li")?,
					Container::DescriptionList => out.write_str("<dl")?,
					Container::DescriptionDetails => out.write_str("<dd")?,
					Container::Table => out.write_str("<table")?,
					Container::TableRow { .. } => out.write_str("<tr")?,
					Container::Section { id } => match self.metadata {
						Some(meta)
							if &meta.title.id == id && matches!(meta.route, PageRoute::Post(_)) =>
						{
							let renderer = template!("templates/post.html", &mut out)?;
							assert_eq!(renderer.current(), Some("attrs"));
							self.states.push(State::Article(renderer));
						}
						_ => out.write_str("<section")?,
					},
					Container::Div {
						class: class @ ("aside" | "long-aside" | "char-aside"),
					} => {
						if *class == "aside" {
							self.list_tightness.push(true);
						}

						let mut renderer = if *class == "aside" {
							template!("templates/aside.html", &mut out)?
						} else if *class == "char-aside" {
							template!("templates/character-aside.html", &mut out)?
						} else {
							template!("templates/long-aside.html", &mut out)?
						};

						while let Some(label) = renderer.current() {
							if label == "character" {
								let character = attrs.get_value("character").ok_or_else(|| {
									anyhow!("Cannot find `character` attribute on `aside` element")
								})?;

								character
									.parts()
									.try_for_each(|part| write_attr(part, &mut out))?;
								renderer.next(&mut out)?;
							} else if label == "title" {
								let title = attrs.get_value("title").ok_or_else(|| {
									anyhow!("Cannot find `title` attribute on `aside` element")
								})?;

								title
									.parts()
									.try_for_each(|part| write_attr(part, &mut out))?;
								renderer.next(&mut out)?;
							} else {
								break;
							}
						}

						self.states.push(State::Aside(renderer));
					}
					Container::Div { .. } => out.write_str("<div")?,
					Container::Heading { level, .. } => write!(out, "<h{}", level)?,
					Container::TableCell { head: false, .. } => out.write_str("<td")?,
					Container::TableCell { head: true, .. } => out.write_str("<th")?,
					Container::Caption => out.write_str("<caption")?,
					Container::Image(..) => out.write_str("<img")?,
					Container::DescriptionTerm => out.write_str("<dt")?,
					Container::CodeBlock { .. } => out.write_str("<pre")?,
					Container::Span | Container::Math { .. } => out.write_str("<span")?,
					Container::Verbatim => out.write_str("<code")?,
					Container::Subscript => out.write_str("<sub")?,
					Container::Superscript => out.write_str("<sup")?,
					Container::Insert => out.write_str("<ins")?,
					Container::Delete => out.write_str("<del")?,
					Container::Strong => out.write_str("<strong")?,
					Container::Emphasis => out.write_str("<em")?,
					Container::Mark => out.write_str("<mark")?,
					Container::LinkDefinition { .. } => return Ok(()),
				}

				let mut write_attribs = true;
				if matches!(
					c,
					Container::Div {
						class: "aside" | "long-aside" | "char-aside"
					}
				) {
					write_attribs = false;
				}

				if write_attribs {
					// {{{ Write attributes
					let mut id_written = false;
					let mut class_written = false;

					if write_attribs {
						for (a, v) in attrs.unique_pairs() {
							let is_class = a == "class";
							let is_id = a == "id";
							if (!is_id || !id_written) && (!is_class || !class_written) {
								write!(out, r#" {}=""#, a)?;
								v.parts().try_for_each(|part| write_attr(part, &mut out))?;
								out.write_char('"')?;

								id_written |= is_id;
								class_written |= is_class;
							};
						}
					}
					// }}}
					// {{{ Write default ids/classes
					if let Container::Heading {
						id,
						has_section: false,
						..
					}
					| Container::Section { id } = &c
					{
						if !id_written {
							out.write_str(r#" id=""#)?;
							write_attr(id, &mut out)?;
							out.write_char('"')?;
						}
					// TODO: do I not want this to add onto the provided class?
					} else if (matches!(c, Container::Div { class } if !class.is_empty())
						|| matches!(
							c,
							Container::Math { .. }
								| Container::List {
									kind: ListKind::Task(..),
									..
								} | Container::TaskListItem { .. }
						)) && !class_written
					{
						out.write_str(r#" class=""#)?;
						write_class(c, false, &mut out)?;
						out.write_char('"')?;
					}
					// }}}

					match c {
						// {{{ Write css for aligning table cell text
						Container::TableCell { alignment, .. }
							if !matches!(alignment, Alignment::Unspecified) =>
						{
							let a = match alignment {
								Alignment::Unspecified => unreachable!(),
								Alignment::Left => "left",
								Alignment::Center => "center",
								Alignment::Right => "right",
							};
							write!(out, r#" style="text-align: {};">"#, a)?;
						}
						// }}}
						// {{{ Write language for codeblock
						Container::CodeBlock { language } => {
							if language.is_empty() {
								out.write_str("><code>")?;
							} else {
								out.write_str(r#"><code class="language-"#)?;
								write_attr(language, &mut out)?;
								out.write_str(r#"">"#)?;
							}
						}
						// }}}
						Container::Image(..) => out.write_str(r#" alt=""#)?,
						Container::Math { display } => {
							out.write_str(r#">"#)?;
							self.states.push(State::Math(*display));
						}
						_ => match self.states.last_mut() {
							Some(State::Article(renderer))
								if renderer.current() == Some("attrs") =>
							{
								renderer.next(&mut out)?;
							}
							_ => out.write_char('>')?,
						},
					}
				}

				match &c {
					Container::Heading { id, .. } => {
						out.write_str(r##"<a href="#"##)?;
						write_attr(id, &mut out)?;
						out.write_str(r#"">◇</a> "#)?;
					}
					Container::Image(..) => {
						self.states.push(State::TextOnly);
					}
					_ => {}
				}
			}
			// }}}
			// {{{ Container end
			Event::End(c) => {
				match &c {
					Container::Image(..) => {
						assert!(matches!(self.states.last(), Some(State::TextOnly)));
						self.states.pop();
					}
					_ => {}
				}

				if matches!(self.states.last(), Some(State::TextOnly)) {
					return Ok(());
				}

				match c {
					Container::RawBlock { .. } => {}
					Container::RawInline { .. } => unreachable!(),
					Container::Footnote { .. } => unreachable!(),
					// {{{ List
					Container::List { kind, .. } => {
						self.list_tightness.pop();
						match kind {
							ListKind::Unordered(..) | ListKind::Task(..) => {
								out.write_str("</ul>")?
							}
							ListKind::Ordered { .. } => out.write_str("</ol>")?,
						}
					}
					// }}}
					// {{{ Paragraph
					Container::Paragraph => {
						if matches!(self.list_tightness.last(), Some(true)) {
							return Ok(());
						}

						if !self.footnotes.in_epilogue() {
							out.write_str("</p>")?;
						}
					}
					// }}}
					// {{{ Image
					Container::Image(src, ..) => {
						if !src.is_empty() {
							out.write_str(r#"" src=""#)?;
							write_attr(src, &mut out)?;
						}

						out.write_str(r#"">"#)?;
					}
					// }}}
					// {{{ Math
					Container::Math { .. } => {
						assert!(matches!(self.states.last(), Some(State::Math(_))));
						self.states.pop();
						out.write_str(r#"</span>"#)?;
					}
					// }}}
					Container::Blockquote => out.write_str("</blockquote>")?,
					Container::ListItem { .. } => out.write_str("</li>")?,
					Container::TaskListItem { .. } => out.write_str("</li>")?,
					Container::DescriptionList => out.write_str("</dl>")?,
					Container::DescriptionDetails => out.write_str("</dd>")?,
					Container::Table => out.write_str("</table>")?,
					Container::TableRow { .. } => out.write_str("</tr>")?,
					Container::Section { id, .. } => match self.metadata {
						Some(meta)
							if &meta.title.id == id
								&& matches!(self.states.last(), Some(State::Article(_))) =>
						{
							let Some(State::Article(renderer)) = self.states.pop() else {
								unreachable!()
							};

							renderer.finish(&mut out)?;
						}
						_ => out.write_str("</section>")?,
					},
					Container::Div {
						class: class @ ("aside" | "long-aside" | "char-aside"),
					} => {
						if *class == "aside" {
							self.list_tightness.pop();
						}

						let state = self.states.pop().unwrap();
						let State::Aside(renderer) = state else {
							panic!("Finished `aside` element without being in the `Aside` state.")
						};

						assert_eq!(renderer.current(), Some("content"));
						renderer.finish(&mut out)?;
					}
					Container::Div { .. } => out.write_str("</div>")?,
					Container::Heading { level, .. } => {
						write!(out, "</h{}>", level)?;

						if let Some(State::Article(renderer)) = self.states.last_mut() {
							if renderer.current() == Some("title") {
								// SAFETY: we can never enter into the `article` state without having
								// some metadata on hand.
								let meta = self.metadata.unwrap();
								while let Some(label) = renderer.next(&mut out)? {
									if label == "posted_on" {
										if let Some(d) = meta.config.created_at {
											write!(&mut out, "Posted on ")?;
											write_datetime(&d, &mut out)?;
										} else {
											write!(&mut out, "Being conjured by ")?;
										}
									} else if label == "updated_on" {
										write_datetime(&meta.last_modified, &mut out)?;
									} else if label == "word_count" {
										let wc = meta.word_count;
										if wc < 400 {
											write!(&mut out, "{}", wc)?;
										} else if wc < 1000 {
											write!(&mut out, "{}", wc / 100 * 100)?;
										} else {
											write!(&mut out, "{} thousand", wc / 1000)?;
										}
									} else if label == "reading_duration" {
										let minutes = meta.word_count / 200;
										if minutes < 10 {
											write!(&mut out, "short {minutes} minute")?;
										} else if minutes < 20 {
											write!(&mut out, "somewhat short {minutes} minute")?;
										} else if minutes < 30 {
											write!(&mut out, "somewhat long {minutes}")?;
										} else if minutes < 60 {
											write!(&mut out, "long {minutes}")?;
										} else {
											let hours = minutes / 60;
											let minutes = minutes % 60;
											write!(
												&mut out,
												"very long {hours} hour and {minutes} minute"
											)?;
										}
									} else if label == "content" {
										break;
									}
								}
							}
						}
					}
					Container::TableCell { head: false, .. } => out.write_str("</td>")?,
					Container::TableCell { head: true, .. } => out.write_str("</th>")?,
					Container::Caption => out.write_str("</caption>")?,
					Container::DescriptionTerm => out.write_str("</dt>")?,
					Container::CodeBlock { .. } => out.write_str("</code></pre>")?,
					Container::Span => out.write_str("</span>")?,
					Container::Link(..) => out.write_str("</a>")?,
					Container::Verbatim => out.write_str("</code>")?,
					Container::Subscript => out.write_str("</sub>")?,
					Container::Superscript => out.write_str("</sup>")?,
					Container::Insert => out.write_str("</ins>")?,
					Container::Delete => out.write_str("</del>")?,
					Container::Strong => out.write_str("</strong>")?,
					Container::Emphasis => out.write_str("</em>")?,
					Container::Mark => out.write_str("</mark>")?,
					Container::LinkDefinition { .. } => unreachable!(),
				}
			}
			// }}}
			// {{{ Raw string
			Event::Str(s) => match self.states.last() {
				Some(State::TextOnly) => write_attr(s, &mut out)?,
				Some(State::Raw) => out.write_str(s)?,
				Some(State::Math(display)) => {
					let config = pulldown_latex::RenderConfig {
						display_mode: {
							use pulldown_latex::config::DisplayMode::*;
							if *display {
								Block
							} else {
								Inline
							}
						},
						annotation: None,
						error_color: (178, 34, 34),
						xml: true,
						math_style: pulldown_latex::config::MathStyle::TeX,
					};

					let mut mathml = String::new();
					let storage = pulldown_latex::Storage::new();
					let parser = pulldown_latex::Parser::new(s, &storage);
					pulldown_latex::push_mathml(&mut mathml, parser, config).unwrap();
					out.write_str(&mathml)?;
				}
				_ => write_text(s, &mut out)?,
			},
			// }}}
			// {{{ Footnote reference
			Event::FootnoteReference(label) => {
				let number = self.footnotes.reference(label);
				if !matches!(self.states.last(), Some(State::TextOnly)) {
					write!(
						out,
						r##"<a id="fnref{}" href="#fn{}" role="doc-noteref"><sup>{}</sup></a>"##,
						number, number, number
					)?;
				}
			}
			// }}}
			// {{{ Symbol
			Event::Symbol(sym) => write!(out, ":{}:", sym)?,
			Event::LeftSingleQuote => out.write_str("‘")?,
			Event::RightSingleQuote => out.write_str("’")?,
			Event::LeftDoubleQuote => out.write_str("“")?,
			Event::RightDoubleQuote => out.write_str("”")?,
			Event::Ellipsis => out.write_str("…")?,
			Event::EnDash => out.write_str("–")?,
			Event::EmDash => out.write_str("—")?,
			Event::NonBreakingSpace => out.write_str("&nbsp;")?,
			Event::Hardbreak => out.write_str("<br>")?,
			Event::Softbreak => out.write_char('\n')?,
			// }}}
			// {{{ Thematic break
			Event::ThematicBreak(attrs) => {
				out.write_str("<hr")?;
				for (a, v) in attrs.unique_pairs() {
					write!(out, r#" {}=""#, a)?;
					v.parts().try_for_each(|part| write_attr(part, &mut out))?;
					out.write_char('"')?;
				}
				out.write_str(">")?;
			}
			// }}}
			Event::Escape | Event::Blankline | Event::Attributes(..) => {}
		}

		Ok(())
	}

	// {{{ Render epilogue
	fn render_epilogue(&mut self, mut out: impl std::fmt::Write) -> anyhow::Result<()> {
		if self.footnotes.reference_encountered() {
			out.write_str("<section role=\"doc-endnotes\">")?;
			out.write_str("<hr>")?;
			out.write_str("<ol>")?;

			while let Some((number, events)) = self.footnotes.next() {
				write!(out, "<li id=\"fn{}\">", number)?;

				let mut unclosed_para = false;
				for e in events.iter().flatten() {
					if matches!(&e, Event::Blankline | Event::Escape) {
						continue;
					}
					if unclosed_para {
						// not a footnote, so no need to add href before para close
						out.write_str("</p>")?;
					}
					self.render_event(e, &mut out)?;
					unclosed_para = matches!(e, Event::End(Container::Paragraph { .. }))
						&& !matches!(self.list_tightness.last(), Some(true));
				}
				if !unclosed_para {
					// create a new paragraph
					out.write_str("<p>")?;
				}
				write!(
					out,
					"<a href=\"#fnref{}\" role=\"doc-backlink\">\u{21A9}\u{FE0E}</a></p>",
					number,
				)?;

				out.write_str("</li>")?;
			}

			out.write_str("</ol>")?;
			out.write_str("</section>")?;
		}

		Ok(())
	}
	// }}}
}

// {{{ Writing helpers
fn write_class<W>(c: &Container, mut first_written: bool, out: &mut W) -> std::fmt::Result
where
	W: std::fmt::Write,
{
	if let Some(cls) = match c {
		Container::List {
			kind: ListKind::Task(..),
			..
		} => Some("task-list"),
		Container::TaskListItem { checked: false } => Some("unchecked"),
		Container::TaskListItem { checked: true } => Some("checked"),
		Container::Math { display: false } => Some("math inline"),
		Container::Math { display: true } => Some("math display"),
		_ => None,
	} {
		first_written = true;
		out.write_str(cls)?;
	}
	if let Container::Div { class } = c {
		if !class.is_empty() {
			if first_written {
				out.write_char(' ')?;
			}
			out.write_str(class)?;
		}
	}
	Ok(())
}

#[inline]
fn write_text(s: &str, out: impl std::fmt::Write) -> std::fmt::Result {
	write_escape(s, false, out)
}

#[inline]
fn write_attr(s: &str, out: impl std::fmt::Write) -> std::fmt::Result {
	write_escape(s, true, out)
}

fn write_escape(
	mut s: &str,
	escape_quotes: bool,
	mut out: impl std::fmt::Write,
) -> std::fmt::Result {
	let mut ent = "";
	while let Some(i) = s.find(|c| {
		match c {
			'<' => Some("&lt;"),
			'>' => Some("&gt;"),
			'&' => Some("&amp;"),
			'"' if escape_quotes => Some("&quot;"),
			_ => None,
		}
		.map_or(false, |s| {
			ent = s;
			true
		})
	}) {
		out.write_str(&s[..i])?;
		out.write_str(ent)?;
		s = &s[i + 1..];
	}
	out.write_str(s)
}

#[inline]
fn write_datetime<T: TimeZone>(
	datetime: &DateTime<T>,
	mut out: impl std::fmt::Write,
) -> std::fmt::Result {
	let datetime = datetime.to_utc();
	write!(
		&mut out,
		"<time datetime={}>{}</time>",
		datetime,
		datetime.format("%a, %d %b %Y")
	)
}
// }}}
// {{{ Footnotes
/// Helper to aggregate footnotes for rendering at the end of the document. It will cache footnote
/// events until they should be emitted at the end.
///
/// When footnotes should be rendered, they can be pulled with the [`Footnotes::next`] function in
/// the order they were first referenced.
#[derive(Default)]
struct Footnotes<'s> {
	/// Stack of current open footnotes, with label and staging buffer.
	open: Vec<(&'s str, Vec<Event<'s>>)>,
	/// Footnote references in the order they were first encountered.
	references: Vec<&'s str>,
	/// Events for each footnote.
	events: HashMap<&'s str, Vec<Event<'s>>>,
	/// Number of last footnote that was emitted.
	number: usize,
}

impl<'s> Footnotes<'s> {
	/// Returns `true` if any reference has been encountered.
	fn reference_encountered(&self) -> bool {
		!self.references.is_empty()
	}

	/// Returns `true` if within the epilogue, i.e. if any footnotes have been pulled.
	fn in_epilogue(&self) -> bool {
		self.number > 0
	}

	/// Add a footnote reference.
	fn reference(&mut self, label: &'s str) -> usize {
		self.references
			.iter()
			.position(|t| *t == label)
			.map_or_else(
				|| {
					self.references.push(label);
					self.references.len()
				},
				|i| i + 1,
			)
	}

	/// Start aggregating a footnote.
	fn start(&mut self, label: &'s str, events: Vec<Event<'s>>) {
		self.open.push((label, events));
	}

	/// Obtain the current (most recently started) footnote.
	fn current(&mut self) -> Option<&mut Vec<Event<'s>>> {
		self.open.last_mut().map(|(_, e)| e)
	}

	/// End the current (most recently started) footnote.
	fn end(&mut self) {
		let (label, stage) = self.open.pop().unwrap();
		self.events.insert(label, stage);
	}
}

impl<'s> Iterator for Footnotes<'s> {
	type Item = (usize, Option<Vec<Event<'s>>>);

	fn next(&mut self) -> Option<Self::Item> {
		self.references.get(self.number).map(|label| {
			self.number += 1;
			(self.number, self.events.remove(label))
		})
	}
}
// }}}
