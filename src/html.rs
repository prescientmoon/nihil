use std::collections::HashMap;
use std::fmt::Display;

use anyhow::anyhow;
use anyhow::bail;
use anyhow::Context;
use chrono::DateTime;
use chrono::NaiveDate;
use chrono::TimeZone;
use jotdown::Alignment;
use jotdown::AttributeValue;
use jotdown::Container;
use jotdown::Event;
use jotdown::LinkType;
use jotdown::ListKind;
use jotdown::OrderedListNumbering::*;
use jotdown::SpanLinkType;
use tree_sitter::Language;
use tree_sitter_highlight::Highlight;
use tree_sitter_highlight::HighlightConfiguration;
use tree_sitter_highlight::HighlightEvent;
use tree_sitter_highlight::Highlighter;

use crate::metadata::has_role;
use crate::metadata::PageMetadata;
use crate::metadata::PageRoute;
use crate::template;
use crate::template::TemplateRenderer;

// {{{ Renderer
/// Render djot content as HTML.
pub fn render_html<'s>(
	metadata: &'s PageMetadata,
	pages: &'s [PageMetadata],
	mut events: impl Iterator<Item = Event<'s>>,
	out: &mut impl std::fmt::Write,
) -> anyhow::Result<()> {
	let mut w = Writer::new(Some(metadata), pages);
	events.try_for_each(|e| w.render_event(&e, out))?;
	w.render_epilogue(out)?;

	Ok(())
}
// }}}

pub struct Writer<'s> {
	list_tightness: Vec<bool>,
	states: Vec<State<'s>>,
	footnotes: Footnotes<'s>,
	metadata: Option<&'s PageMetadata<'s>>,
	pages: &'s [PageMetadata<'s>],
}

#[derive(Debug, Clone)]
enum State<'s> {
	TextOnly,
	Ignore,
	Raw,
	Math(bool),
	CodeBlock(String),
	Aside(TemplateRenderer<'s>),
	Article(TemplateRenderer<'s>),
	Footnote(Vec<jotdown::Event<'s>>),
	Datetime(String),
	Date(String),
}

impl<'s> Writer<'s> {
	pub fn new(metadata: Option<&'s PageMetadata>, pages: &'s [PageMetadata]) -> Self {
		Self {
			list_tightness: Vec::new(),
			states: Vec::new(),
			footnotes: Footnotes::default(),
			metadata,
			pages,
		}
	}

	#[allow(clippy::single_match)]
	pub fn render_event(
		&mut self,
		e: &Event<'s>,
		out: &mut impl std::fmt::Write,
	) -> anyhow::Result<()> {
		// {{{ Handle "footnote" states
		if matches!(self.states.last(), Some(State::Footnote(_))) {
			if let Event::End(Container::Footnote { label }) = e {
				let Some(State::Footnote(events)) = self.states.pop() else {
					unreachable!()
				};

				self.footnotes.insert(label, events);
			} else {
				let Some(State::Footnote(events)) = self.states.last_mut() else {
					unreachable!()
				};

				events.push(e.clone());
				return Ok(());
			}
		}
		// }}}
		// {{{ Handle "text-only" states
		if matches!(self.states.last(), Some(State::TextOnly)) {
			match e {
				Event::End(Container::Image(..)) => {
					self.states.pop();
				}
				Event::Str(s) => write!(out, "{}", Escaped(s))?,
				_ => return Ok(()),
			}
		}
		// }}}
		// {{{ Handle "ignore" states
		if matches!(self.states.last(), Some(State::Ignore)) {
			match e {
				Event::End(
					Container::RawBlock { .. }
					| Container::RawInline { .. }
					| Container::LinkDefinition { .. }
					| Container::Div { .. },
				) => {
					self.states.pop();
					return Ok(());
				}
				_ => return Ok(()),
			}
		}
		// }}}

		match e {
			// {{{ Container start
			Event::Start(c, attrs) => {
				match &c {
					// {{{ Section
					Container::Section { id } => match self.metadata {
						Some(meta) if meta.title.id == *id => {
							if matches!(meta.route, PageRoute::Post(_)) {
								let mut renderer = template!("templates/post.html", out)?;

								assert_eq!(renderer.current(), Some("attrs"));
								write!(out, "{}", Attr("aria-labeledby", id))?;
								renderer.next(out)?;

								self.states.push(State::Article(renderer));
							}
						}
						_ => {
							write!(out, "<section {}>", Attr("aria-labeledby", id))?;
						}
					},
					// }}}
					// {{{ Aside
					Container::Div {
						class: class @ ("aside" | "long-aside" | "char-aside"),
					} => {
						let mut renderer = if *class == "aside" {
							template!("templates/aside.html", out)?
						} else if *class == "char-aside" {
							template!("templates/character-aside.html", out)?
						} else {
							template!("templates/long-aside.html", out)?
						};

						while let Some(label) = renderer.current() {
							match label {
								"character" => {
									let character =
										attrs.get_value("character").ok_or_else(|| {
											anyhow!("Cannot find `character` attribute on `aside` element")
										})?;

									write_attribute(out, &character)?;
								}
								"title" => {
									let title = attrs.get_value("title").ok_or_else(|| {
										anyhow!("Cannot find `title` attribute on `aside` element")
									})?;

									write_attribute(out, &title)?;
								}
								_ => break,
							}
							renderer.next(out)?;
						}

						self.states.push(State::Aside(renderer));
					}
					// }}}
					// {{{ List
					Container::List { kind, tight } => {
						self.list_tightness.push(*tight);
						match kind {
							ListKind::Unordered(..) => out.write_str("<ul>")?,
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

								write!(out, ">")?;
							}
							ListKind::Task(_) => bail!("Task lists are not supported"),
						}
					}
					// }}}
					// {{{ Link
					Container::Link(dst, ty) => {
						if matches!(ty, LinkType::Span(SpanLinkType::Unresolved)) {
							bail!("Unresolved url {dst:?}")
						} else {
							let prefix = if matches!(ty, LinkType::Email) {
								"mailto:"
							} else {
								""
							};

							write!(out, r#"<a href="{prefix}{}">"#, Escaped(dst))?;
						}
					}
					// }}}
					// {{{ Table cell
					Container::TableCell {
						head, alignment, ..
					} => {
						if *head {
							out.write_str("<td")?;
						} else {
							out.write_str("<th")?;
						}

						if !matches!(alignment, Alignment::Unspecified) {
							// TODO: move this to css
							let a = match alignment {
								Alignment::Unspecified => unreachable!(),
								Alignment::Left => "left",
								Alignment::Center => "center",
								Alignment::Right => "right",
							};

							write!(out, r#" style="text-align: {};""#, a)?;
						}

						write!(out, ">")?;
					}
					// }}}
					// {{{ Heading
					Container::Heading { level, id, .. } => {
						write!(
							out,
							r##"<h{level} id="{}"><a class="heading-anchor" href="#{}">◇</a> "##,
							Escaped(id),
							Escaped(id)
						)?;
					}
					// }}}
					// {{{ Paragraph
					Container::Paragraph => {
						if self.list_tightness.last() == Some(&true) {
							return Ok(());
						}

						out.write_str("<p>")?;
					}
					// }}}
					// {{{ Post list
					Container::Div { class: "posts" } => {
						write!(out, r#"<ol class="article-list">"#)?;
						for post in self.pages {
							// Skip drafts
							if post.config.created_at.is_none() {
								continue;
							}

							template!("templates/post-summary.html", out)?.feed(
								out,
								|label, out| {
									match label {
										"id" => {
											if let PageRoute::Post(id) = &post.route {
												write!(out, "{id}")?;
											}
										}
										"title" => {
											for event in &post.title.events {
												self.render_event(event, out)?;
											}
										}
										"description" => {
											for event in &post.description {
												self.render_event(event, out)?;
											}
										}
										_ => {
											if !fill_metadata_label(out, label, post)? {
												bail!("Unknown label {label} in `post-summary` template");
											};
										}
									}

									Ok(true)
								},
							)?;
						}
						write!(out, "</ol>")?;
						// We don't care about the contents of this block
						self.states.push(State::Ignore);
					}
					// }}}
					// {{{ Div
					Container::Div { class } => {
						if has_role(attrs, "description") {
							self.states.push(State::Ignore);
						} else {
							write!(out, "<div{}>", Attr("class", class))?;
						}
					}
					// }}}
					// {{{ Raw block
					Container::RawBlock { format } | Container::RawInline { format } => {
						if format == &"html" {
							self.states.push(State::Raw);
						} else {
							self.states.push(State::Ignore);
						};
					}
					// }}}
					Container::CodeBlock { .. } => {
						self.states.push(State::CodeBlock(String::new()));
						out.write_str("<pre><code>")?;
					}
					Container::Math { display } => {
						self.states.push(State::Math(*display));
						out.write_str("<math>")?;
					}
					Container::Image(_, _) => {
						self.states.push(State::TextOnly);
						out.write_str(r#"<img alt=""#)?;
					}
					Container::Footnote { .. } => self.states.push(State::Footnote(Vec::new())),
					Container::LinkDefinition { .. } => self.states.push(State::Ignore),
					Container::Blockquote => out.write_str("<blockquote>")?,
					Container::ListItem { .. } => out.write_str("<li>")?,
					Container::DescriptionList => out.write_str("<dl>")?,
					Container::DescriptionDetails => out.write_str("<dd>")?,
					Container::Table => out.write_str("<table>")?,
					Container::TableRow { .. } => out.write_str("<tr>")?,
					Container::Caption => out.write_str("<caption>")?,
					Container::DescriptionTerm => out.write_str("<dt>")?,
					Container::Span => {
						if has_role(attrs, "datetime") {
							self.states.push(State::Datetime(String::new()))
						} else if has_role(attrs, "date") {
							self.states.push(State::Date(String::new()))
						} else {
							out.write_str("<span>")?
						}
					}
					Container::Verbatim => out.write_str("<code>")?,
					Container::Subscript => out.write_str("<sub>")?,
					Container::Superscript => out.write_str("<sup>")?,
					Container::Insert => out.write_str("<ins>")?,
					Container::Delete => out.write_str("<del>")?,
					Container::Strong => out.write_str("<strong>")?,
					Container::Emphasis => out.write_str("<em>")?,
					Container::Mark => out.write_str("<mark>")?,
					e => bail!("DJot element {e:?} is not supported"),
				}
			}
			// }}}
			// {{{ Container end
			Event::End(c) => {
				match c {
					Container::Footnote { .. } => unreachable!(),
					// {{{ Raw block
					Container::RawBlock { .. } | Container::RawInline { .. } => {
						// Sanity check
						assert!(matches!(self.states.last(), Some(State::Raw)));

						self.states.pop();
					}
					// }}}
					// {{{ List
					Container::List { kind, .. } => {
						self.list_tightness.pop();
						match kind {
							ListKind::Unordered(..) => out.write_str("</ul>")?,
							ListKind::Ordered { .. } => out.write_str("</ol>")?,
							// We error out when the task list begins
							ListKind::Task(..) => unreachable!(),
						}
					}
					// }}}
					// {{{ Paragraph
					Container::Paragraph => {
						if matches!(self.list_tightness.last(), Some(true)) {
							return Ok(());
						}

						out.write_str("</p>")?;
					}
					// }}}
					// {{{ Math
					Container::Math { .. } => {
						// Sanity check
						assert!(matches!(self.states.last(), Some(State::Math(_))));
						self.states.pop();
						out.write_str(r#"</math>"#)?;
					}
					// }}}
					// {{{ Section
					Container::Section { id, .. } => match self.metadata {
						Some(meta) if meta.title.id == *id => {
							if matches!(self.states.last(), Some(State::Article(_))) {
								let Some(State::Article(renderer)) = self.states.pop() else {
									unreachable!()
								};

								renderer.finish(out)?;
							}
						}
						_ => out.write_str("</section>")?,
					},
					// }}}
					// {{{ Aside
					Container::Div {
						class: "aside" | "long-aside" | "char-aside",
					} => {
						let state = self.states.pop().unwrap();
						let State::Aside(renderer) = state else {
							panic!("Finished `aside` element without being in the `Aside` state.")
						};

						renderer.finish(out)?;
					}
					// }}}
					Container::Heading { level, .. } => {
						write!(out, "</h{}>", level)?;

						// {{{ Article title
						if let Some(State::Article(renderer)) = self.states.last_mut() {
							if renderer.current() == Some("title") {
								// SAFETY: we can never enter into the `article` state without having
								// some metadata on hand.
								let meta = self.metadata.unwrap();
								while let Some(label) = renderer.next(out)? {
									if !fill_metadata_label(out, label, meta)? {
										break;
									}
								}
							}
						}
						// }}}
					}
					Container::Image(src, ..) => write!(out, r#" {}>"#, Attr("src", src))?,
					Container::Blockquote => out.write_str("</blockquote>")?,
					Container::ListItem { .. } => out.write_str("</li>")?,
					Container::DescriptionList => out.write_str("</dl>")?,
					Container::DescriptionDetails => out.write_str("</dd>")?,
					Container::Table => out.write_str("</table>")?,
					Container::TableRow { .. } => out.write_str("</tr>")?,
					Container::Div { .. } => out.write_str("</div>")?,
					Container::TableCell { head: false, .. } => out.write_str("</td>")?,
					Container::TableCell { head: true, .. } => out.write_str("</th>")?,
					Container::Caption => out.write_str("</caption>")?,
					Container::DescriptionTerm => out.write_str("</dt>")?,
					// {{{ Syntax highlighting
					Container::CodeBlock { language } => {
						let Some(State::CodeBlock(buffer)) = self.states.pop() else {
							panic!("Arrived at end of code block without being in the approriate state.");
						};

						if *language == "rust" {
							let mut highlighter = Highlighter::new();
							let language = Language::new(tree_sitter_rust::LANGUAGE);

							let mut config = HighlightConfiguration::new(
								language,
								"rust",
								tree_sitter_rust::HIGHLIGHTS_QUERY,
								tree_sitter_rust::INJECTIONS_QUERY,
								"",
							)?;

							let highlight_names = [
								"attribute",
								"comment",
								"comment.documentation",
								"constant",
								"constant.builtin",
								"constructor",
								"function",
								"function.builtin",
								"function.macro",
								"function.method",
								"keyword",
								"label",
								"operator",
								"property",
								"punctuation",
								"punctuation.bracket",
								"punctuation.delimiter",
								"string",
								"string.special",
								"tag",
								"type",
								"type.builtin",
								"variable",
								"variable.builtin",
								"variable.parameter",
							];

							let highlight_classes = highlight_names
								.iter()
								.map(|s| s.replace(".", "-"))
								.collect::<Vec<_>>();

							config.configure(&highlight_names);

							let highlights =
								highlighter
									.highlight(&config, buffer.as_bytes(), None, |_| None)?;

							for event in highlights {
								match event? {
									HighlightEvent::Source { start, end } => {
										write!(out, "{}", Escaped(&buffer[start..end]))?;
									}
									HighlightEvent::HighlightStart(Highlight(index)) => {
										write!(
											out,
											r#"<span class="{}">"#,
											highlight_classes[index]
										)?;
									}
									HighlightEvent::HighlightEnd => {
										write!(out, r#"</span>"#)?;
									}
								}
							}
						} else {
							write!(out, "{}", Escaped(&buffer))?;
						}

						out.write_str("</code></pre>")?
					}
					// }}}
					Container::Span => {
						if matches!(self.states.last(), Some(State::Datetime(_))) {
							let Some(State::Datetime(buffer)) = self.states.pop() else {
								unreachable!()
							};

							write_datetime(out, &DateTime::parse_from_rfc3339(&buffer)?)?;
						} else if matches!(self.states.last(), Some(State::Date(_))) {
							let Some(State::Date(buffer)) = self.states.pop() else {
								unreachable!()
							};

							let date = NaiveDate::parse_from_str(&buffer, "%Y-%m-%d")
								.with_context(|| "Failed to parse date inside span")?;
							write!(
								out,
								r#"<time datetime="{}">{}</time>"#,
								date.format("%Y-%m-%d"),
								date.format("%a, %d %b %Y")
							)?;
						} else {
							out.write_str("</span>")?;
						}
					}

					Container::Link(..) => out.write_str("</a>")?,
					Container::Verbatim => out.write_str("</code>")?,
					Container::Subscript => out.write_str("</sub>")?,
					Container::Superscript => out.write_str("</sup>")?,
					Container::Insert => out.write_str("</ins>")?,
					Container::Delete => out.write_str("</del>")?,
					Container::Strong => out.write_str("</strong>")?,
					Container::Emphasis => out.write_str("</em>")?,
					Container::Mark => out.write_str("</mark>")?,
					e => bail!("DJot element {e:?} is not supported"),
				}
			}
			// }}}
			// {{{ Raw string
			Event::Str(s) => match self.states.last_mut() {
				Some(State::Raw) => out.write_str(s)?,
				Some(State::CodeBlock(buffer) | State::Datetime(buffer) | State::Date(buffer)) => {
					buffer.push_str(s)
				}
				// {{{ Math
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
				// }}}
				_ => write!(out, "{}", Escaped(s))?,
			},
			// }}}
			// {{{ Footnote reference
			Event::FootnoteReference(label) => {
				let number = self.footnotes.reference(label);
				if !matches!(self.states.last(), Some(State::TextOnly)) {
					write!(
						out,
						r##"
              <sup>
                <a id="fnref{number}" href="#fn{number}" role="doc-noteref">
                  {number}
                </a>
              </sup>
            "##
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
			Event::ThematicBreak(_) => out.write_str("<hr>")?,
			Event::Escape | Event::Blankline | Event::Attributes(_) => {}
		}

		Ok(())
	}

	// {{{ Render epilogue
	fn render_epilogue(&mut self, out: &mut impl std::fmt::Write) -> anyhow::Result<()> {
		if self.footnotes.reference_encountered() {
			// TODO: rewrite this using a template
			out.write_str("<section role=\"doc-endnotes\"><hr><ol>")?;

			while let Some((number, events)) = self.footnotes.next() {
				write!(out, r#"<li id="fn{number}">"#)?;

				for e in events.iter().flatten() {
					self.render_event(e, out)?;
				}

				write!(
					out,
					r##"
            <a href="#fnref{number}" role="doc-backlink">
                Return to content ↩︎
            </a></li>
          "##,
				)?;
			}

			out.write_str("</ol></section>")?;
		}

		Ok(())
	}
	// }}}
}

// {{{ HTMl escaper
pub struct Escaped<'a>(&'a str);

impl<'s> Display for Escaped<'s> {
	fn fmt(&self, out: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		let mut s = self.0;
		let mut ent = "";
		while let Some(i) = s.find(|c| {
			match c {
				'<' => Some("&lt;"),
				'>' => Some("&gt;"),
				'&' => Some("&amp;"),
				'"' => Some("&quot;"),
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
}
// }}}
// {{{ Render attributes
pub struct Attr<'a>(&'static str, &'a str);

impl<'s> Display for Attr<'s> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		if !self.1.is_empty() {
			write!(f, r#" {}="{}""#, self.0, Escaped(self.1))?;
		}

		Ok(())
	}
}
// }}}
// {{{ Render datetimes
#[inline]
fn write_datetime<T: TimeZone>(
	out: &mut impl std::fmt::Write,
	datetime: &DateTime<T>,
) -> std::fmt::Result {
	let datetime = datetime.to_utc();
	write!(
		out,
		r#"<time datetime="{}">{}</time>"#,
		datetime.to_rfc3339(),
		datetime.format("%a, %d %b %Y")
	)
}
// }}}
// {{{ Jotdown attribute helpers
#[inline]
fn write_attribute(out: &mut impl std::fmt::Write, attr: &AttributeValue) -> std::fmt::Result {
	attr.parts()
		.try_for_each(|part| write!(out, "{}", Escaped(part)))
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

	/// Insert a new footnote to be renderer later
	fn insert(&mut self, label: &'s str, events: Vec<jotdown::Event<'s>>) {
		self.events.insert(label, events);
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
// {{{ Fill in metadata labels
fn fill_metadata_label(
	out: &mut impl std::fmt::Write,
	label: &str,
	meta: &PageMetadata<'_>,
) -> anyhow::Result<bool> {
	if label == "posted_on" {
		if let Some(d) = meta.config.created_at {
			write!(out, "Posted on ")?;
			write_datetime(out, &d)?;
		} else {
			write!(out, "Being conjured by ")?;
		}
	} else if label == "updated_on" {
		write_datetime(out, &meta.last_modified)?;
	} else if label == "word_count" {
		let wc = meta.word_count;
		if wc < 400 {
			write!(out, "{}", wc)?;
		} else if wc < 1000 {
			write!(out, "{}", wc / 10 * 10)?;
		} else if wc < 2000 {
			write!(out, "{}", wc / 100 * 100)?;
		} else {
			write!(out, "{} thousand", wc / 1000)?;
		}
	} else if label == "reading_duration" {
		let minutes = meta.word_count / 200;
		if minutes == 0 {
			let seconds = meta.word_count * 60 / 200;
			write!(out, "very short {seconds} second")?;
		} else if minutes < 10 {
			write!(out, "short {minutes} minute")?;
		} else if minutes < 20 {
			write!(out, "somewhat short {minutes} minute")?;
		} else if minutes < 30 {
			write!(out, "somewhat long {minutes}")?;
		} else if minutes < 60 {
			write!(out, "long {minutes}")?;
		} else {
			let hours = minutes / 60;
			let minutes = minutes % 60;
			write!(out, "very long {hours} hour and {minutes} minute")?;
		}
	} else {
		return Ok(false);
	}

	Ok(true)
}

// }}}
