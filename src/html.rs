//! An HTML renderer that takes an iterator of [`Event`]s and emits HTML.

use std::collections::HashMap;

use jotdown::Alignment;
use jotdown::Container;
use jotdown::Event;
use jotdown::LinkType;
use jotdown::ListKind;
use jotdown::OrderedListNumbering::*;
use jotdown::Render;
use jotdown::RenderRef;
use jotdown::SpanLinkType;

// {{{ Renderer
/// Render events into a string.
pub fn render_to_string<'s, I>(events: I) -> String
where
	I: Iterator<Item = Event<'s>>,
{
	let mut s = String::new();
	Renderer::default().push(events, &mut s).unwrap();
	s
}

/// [`Render`] implementor that writes HTML output.
#[derive(Clone, Default)]
pub struct Renderer {}

impl Render for Renderer {
	fn push<'s, I, W>(&self, mut events: I, mut out: W) -> std::fmt::Result
	where
		I: Iterator<Item = Event<'s>>,
		W: std::fmt::Write,
	{
		let mut w = Writer::new();
		events.try_for_each(|e| w.render_event(&e, &mut out))?;
		w.render_epilogue(&mut out)
	}
}

impl RenderRef for Renderer {
	fn push_ref<'s, E, I, W>(&self, mut events: I, mut out: W) -> std::fmt::Result
	where
		E: AsRef<Event<'s>>,
		I: Iterator<Item = E>,
		W: std::fmt::Write,
	{
		let mut w = Writer::new();
		events.try_for_each(|e| w.render_event(e.as_ref(), &mut out))?;
		w.render_epilogue(&mut out)
	}
}
// }}}

struct Writer<'s> {
	list_tightness: Vec<bool>,
	states: Vec<State>,
	footnotes: Footnotes<'s>,
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
enum State {
	TextOnly,
	Ignore,
	Raw,
	Math(bool),
}

impl<'s> Writer<'s> {
	fn new() -> Self {
		Self {
			list_tightness: Vec::new(),
			states: Vec::new(),
			footnotes: Footnotes::default(),
		}
	}

	#[allow(clippy::single_match)]
	fn render_event<W>(&mut self, e: &Event<'s>, mut out: W) -> std::fmt::Result
	where
		W: std::fmt::Write,
	{
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
				assert_eq!(self.states.last(), Some(&State::Ignore));
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
					assert_eq!(self.states.last(), Some(&State::Raw));
				} else {
					assert_eq!(self.states.last(), Some(&State::Ignore));
				};

				self.states.pop();
			}

			_ => {}
		}
		// }}}

		if self.states.last() == Some(&State::Ignore) {
			return Ok(());
		}

		match e {
			// {{{ Container start
			Event::Start(c, attrs) => {
				if self.states.last() == Some(&State::TextOnly) {
					return Ok(());
				}

				match &c {
					Container::RawBlock { .. } => unreachable!(),
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
					Container::Section { .. } => out.write_str("<section")?,
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

				// {{{ Write attributes
				let mut id_written = false;
				let mut class_written = false;

				for (a, v) in attrs.unique_pairs() {
					write!(out, r#" {}=""#, a)?;
					v.parts().try_for_each(|part| write_attr(part, &mut out))?;
					match a {
						"class" => {
							class_written = true;
							write_class(c, true, &mut out)?;
						}
						"id" => id_written = true,
						_ => {}
					}
					out.write_char('"')?;
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
					_ => out.write_char('>')?,
				}

				match &c {
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
						assert_eq!(self.states.last(), Some(&State::TextOnly));
						self.states.pop();
					}
					_ => {}
				}

				if self.states.last() == Some(&State::TextOnly) {
					return Ok(());
				}

				match c {
					Container::RawBlock { .. } => unreachable!(),
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
					Container::Section { .. } => out.write_str("</section>")?,
					Container::Div { .. } => out.write_str("</div>")?,
					Container::Heading { level, .. } => write!(out, "</h{}>", level)?,
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
					// let string: String = format!("{}{s}{}", delim, delim);
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
				if self.states.last() != Some(&State::TextOnly) {
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
	fn render_epilogue<W>(&mut self, mut out: W) -> std::fmt::Result
	where
		W: std::fmt::Write,
	{
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

fn write_text<W>(s: &str, out: W) -> std::fmt::Result
where
	W: std::fmt::Write,
{
	write_escape(s, false, out)
}

fn write_attr<W>(s: &str, out: W) -> std::fmt::Result
where
	W: std::fmt::Write,
{
	write_escape(s, true, out)
}

fn write_escape<W>(mut s: &str, escape_quotes: bool, mut out: W) -> std::fmt::Result
where
	W: std::fmt::Write,
{
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
