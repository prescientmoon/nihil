#![allow(dead_code)]
use std::fmt::Write;

use anyhow::{anyhow, bail, Context};
use jotdown::{Container, Event};
use serde::Deserialize;

use crate::html;

#[derive(Deserialize, Debug, Default)]
pub struct PageConfig {
	created_at: Option<String>,
}

impl PageConfig {
	/// Merge another config into the current one. Might error out on duplicate values.
	fn merge(&mut self, other: PageConfig) -> anyhow::Result<()> {
		match &self.created_at {
			None => self.created_at = other.created_at,
			Some(first_created) => {
				if let Some(second_created) = other.created_at {
					if second_created != *first_created {
						bail!("Conflicting values for `created_at` page attribute: {first_created} and {second_created}");
					}
				}
			}
		};

		Ok(())
	}
}

#[derive(Debug)]
pub struct PageMetadata {
	title: Heading,
	config: PageConfig,
	toc: Vec<Heading>,
}

impl PageMetadata {
	pub fn new<'s>(mut events: impl Iterator<Item = Event<'s>>) -> anyhow::Result<Self> {
		let mut w = Writer::new();
		events.try_for_each(|e| w.render_event(&e))?;

		let title = w
			.toc
			.first()
			.ok_or_else(|| anyhow!("No heading found to infer title from"))?;

		Ok(Self {
			title: title.to_owned(),
			config: w.config,
			toc: w.toc,
		})
	}
}

#[derive(Debug, Clone)]
struct Heading {
	pub level: u8,
	pub id: String,
	pub text: String,
	pub html: String,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum State {
	Toplevel,
	Heading,
	Toml,
}

struct Writer<'s> {
	html_renderer: html::Writer<'s>,
	config: PageConfig,
	toc: Vec<Heading>,
	toml_text: String,
	state: State,
}

impl<'s> Writer<'s> {
	fn new() -> Self {
		Self {
			html_renderer: html::Writer::new(),
			config: PageConfig::default(),
			toc: Vec::new(),
			toml_text: String::new(),
			state: State::Toplevel,
		}
	}

	fn render_event(&mut self, e: &jotdown::Event<'s>) -> anyhow::Result<()> {
		match e {
			Event::Start(Container::Heading { level, id, .. }, _) => {
				assert_eq!(self.state, State::Toplevel);
				self.state = State::Heading;
				self.toc.push(Heading {
					level: *level as u8,
					id: id.to_string(),
					text: String::new(),
					html: String::new(),
				})
			}
			Event::End(Container::Heading { .. }) => {
				assert_eq!(self.state, State::Heading);
				self.state = State::Toplevel;
			}
			Event::Start(Container::RawBlock { format: "toml" }, attrs) => {
				assert_eq!(self.state, State::Toplevel);
				if let Some(role_attr) = attrs.get_value("role") {
					if format!("{}", role_attr) == "config" {
						self.state = State::Toml
					}
				}
			}
			Event::End(Container::RawBlock { format: "toml" }) => {
				if self.state == State::Toml {
					self.state = State::Toplevel;

					let config: PageConfig = toml::from_str(&self.toml_text)
						.with_context(|| "Failed to parse page config in TOML format")?;
					self.config.merge(config)?;

					self.toml_text.clear();
				}
			}
			Event::Str(str) if self.state == State::Toml => {
				self.toml_text.write_str(str)?;
			}
			other if self.state == State::Heading => {
				let last_heading = self.toc.last_mut().unwrap();
				self.html_renderer.render_event(e, &mut last_heading.html)?;

				if let Event::Str(str) = other {
					last_heading.text.write_str(str)?;
				}
			}
			_ => {}
		}

		Ok(())
	}
}
