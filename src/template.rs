use anyhow::bail;

// {{{ Templates & stops
#[derive(Clone, Debug)]
struct Stop<'s> {
	label: &'s str,
	start: usize,
	length: usize,
}

#[derive(Clone, Debug)]
pub struct Template<'s> {
	text: &'s str,
	stops: Vec<Stop<'s>>,
}

impl<'s> Template<'s> {
	// {{{ Parse template
	#[allow(clippy::iter_nth_zero)]
	pub fn parse(text: &'s str) -> anyhow::Result<Template<'s>> {
		let mut stops = Vec::new();

		let open_stop = "{{";
		let close_stop = "}}";

		let mut current_stop: Option<Stop> = None;
		let mut prev_ix = None;
		for (ix, c) in text.char_indices() {
			let l = c.len_utf8();
			if let Some(prev) = prev_ix {
				// This char, together with the previous one
				let last_two = &text[prev..ix + l];
				if close_stop == last_two {
					if let Some(mut stop) = current_stop.take() {
						// I think this is safe, as { and } are ascii
						stop.label = &text[stop.start + 2..=ix - 2];
						stop.length = ix + 1 - stop.start;
						stops.push(stop);
					}
				} else if open_stop == last_two && current_stop.is_none() {
					current_stop = Some(Stop {
						label: "",
						start: prev,
						length: 0,
					});
				}
			}

			prev_ix = Some(ix);
		}

		Ok(Template { text, stops })
	}
	// }}}
}
// }}}
// {{{ Template rendering
#[derive(Clone, Debug)]
pub struct TemplateRenderer<'a> {
	template: &'a Template<'a>,
	stop_index: Option<usize>,
}

impl<'a> TemplateRenderer<'a> {
	#[inline]
	pub fn start(template: &'a Template, mut w: impl std::fmt::Write) -> anyhow::Result<Self> {
		let stop_index = if !template.stops.is_empty() {
			Some(0)
		} else {
			None
		};

		let result = Self {
			template,
			stop_index,
		};

		let (next_pos, _) = result.current_stop_range();
		w.write_str(&template.text[..next_pos])?;

		Ok(result)
	}

	/// Get the current placeholder label
	pub fn current(&self) -> Option<&'a str> {
		self.stop_index.map(|ix| self.template.stops[ix].label)
	}

	/// Attempt to finish rendering.
	pub fn finish(mut self, w: impl std::fmt::Write) -> anyhow::Result<()> {
		if let Some(label) = self.next(w)? {
			bail!("Attempting to finish template rendering before label `{label}` was handled");
		}

		Ok(())
	}

	// {{{ Advance to the next placeholder
	/// Move onto the next placeholder
	pub fn next(&mut self, mut w: impl std::fmt::Write) -> anyhow::Result<Option<&'a str>> {
		let Some(stop_index) = self.stop_index else {
			return Ok(None);
		};

		let (_, current_pos) = self.current_stop_range();

		let next_stop_ix = stop_index + 1;

		self.stop_index = if next_stop_ix < self.template.stops.len() {
			Some(next_stop_ix)
		} else {
			None
		};

		let (next_pos, _) = self.current_stop_range();
		w.write_str(&self.template.text[current_pos..next_pos])?;

		Ok(self.current())
	}

	fn current_stop_range(&self) -> (usize, usize) {
		match self.stop_index {
			Some(stop_ix) => {
				let stop = &self.template.stops[stop_ix];
				(stop.start, stop.start + stop.length)
			}
			None => (self.template.text.len(), self.template.text.len()),
		}
	}
	// }}}
}
// }}}
// {{{ Macro
#[macro_export]
macro_rules! template {
	($path:literal,$w:expr) => {{
		use once_cell::sync::OnceCell;
		use $crate::template::{Template, TemplateRenderer};

		static TEMPLATE_TEXT: &str = include_str!($path);
		static CELL: OnceCell<Template<'static>> = OnceCell::new();

		CELL.get_or_try_init(|| Template::parse(TEMPLATE_TEXT))
			.and_then(|t| TemplateRenderer::start(t, $w))
	}};
}
// }}}
