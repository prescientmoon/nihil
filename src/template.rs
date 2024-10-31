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
		for (ix, _) in text.char_indices() {
			if let Some(prev) = prev_ix {
				// This char, together with the previous one
				let last_two = &text[prev..=ix];
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
#[derive(PartialEq, Eq, Clone, Copy, Debug)]
enum RendererState {
	Started,
	InStop(usize),
	Finished,
}

#[derive(Clone, Debug)]
pub struct TemplateRenderer<'a> {
	template: &'a Template<'a>,
	state: RendererState,
}

impl<'a> TemplateRenderer<'a> {
	#[inline]
	pub fn new(template: &'a Template) -> Self {
		Self {
			template,
			state: RendererState::Started,
		}
	}

	/// Get the current placeholder label
	pub fn current(&mut self, w: impl std::fmt::Write) -> anyhow::Result<Option<&'a str>> {
		let current_label = match self.state {
			RendererState::Started => self.next(w)?,
			RendererState::InStop(ix) => Some(self.template.stops[ix].label),
			RendererState::Finished => None,
		};

		Ok(current_label)
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
		if self.state == RendererState::Finished {
			return Ok(None);
		}

		let (_, current_pos) = self.current_stop_range();

		let next_stop_ix = match self.state {
			RendererState::Started => 0,
			RendererState::InStop(stop_ix) => stop_ix + 1,
			RendererState::Finished => unreachable!(),
		};

		self.state = if next_stop_ix < self.template.stops.len() {
			RendererState::InStop(next_stop_ix)
		} else {
			RendererState::Finished
		};

		let (next_pos, _) = self.current_stop_range();
		w.write_str(&self.template.text[current_pos..next_pos])?;

		let current_label = match self.state {
			RendererState::InStop(ix) => Some(self.template.stops[ix].label),
			_ => None,
		};

		Ok(current_label)
	}

	fn current_stop_range(&self) -> (usize, usize) {
		match self.state {
			RendererState::Started => (0, 0),
			RendererState::InStop(stop_ix) => {
				let stop = &self.template.stops[stop_ix];
				(stop.start, stop.start + stop.length)
			}
			RendererState::Finished => (self.template.text.len(), self.template.text.len()),
		}
	}

	// }}}
}
// }}}
// {{{ Macro
#[macro_export]
macro_rules! template {
	($path:literal) => {{
		use once_cell::sync::OnceCell;
		use $crate::template::Template;

		static TEMPLATE_TEXT: &str = include_str!($path);
		static CELL: OnceCell<Template<'static>> = OnceCell::new();

		CELL.get_or_try_init(|| Template::parse(TEMPLATE_TEXT))
	}};
}
// }}}
