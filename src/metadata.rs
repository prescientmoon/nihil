use std::ffi::OsStr;
use std::fmt::Write;
use std::path::{Component, Path, PathBuf};
use std::process::Command;

use anyhow::{anyhow, bail, Context};
use chrono::{DateTime, FixedOffset};
use jotdown::{Container, Event};
use serde::Deserialize;

use crate::html;

#[derive(Deserialize, Debug, Default)]
pub struct PageConfig {
	pub created_at: Option<DateTime<FixedOffset>>,
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
pub enum PageRoute {
	Home,
	Posts,
	#[allow(dead_code)]
	Post(String),
}

impl PageRoute {
	fn from_path(path: &Path) -> anyhow::Result<Self> {
		let Some(Component::Normal(first)) = path.components().nth(1) else {
			bail!("Path is too short");
		};

		let result = if first == OsStr::new("index.dj") {
			Self::Home
		} else if first == OsStr::new("posts") {
			if let Some(Component::Normal(second)) = path.components().nth(2) {
				let mut slice = second.to_str().unwrap();
				if slice.ends_with(".dj") {
					slice = slice.strip_suffix(".dj").unwrap();
				}

				Self::Post(slice.to_owned())
			} else {
				Self::Posts
			}
		} else {
			bail!("Cannot convert path '{:?}' to page route", path);
		};

		Ok(result)
	}
}

#[derive(Debug)]
pub struct PageMetadata {
	pub title: Heading,
	pub config: PageConfig,
	pub word_count: usize,
	pub last_modified: DateTime<FixedOffset>,
	pub route: PageRoute,

	#[allow(dead_code)]
	pub toc: Vec<Heading>,
	#[allow(dead_code)]
	pub path: PathBuf,
}

impl PageMetadata {
	pub fn new<'s>(
		mut events: impl Iterator<Item = Event<'s>>,
		path: PathBuf,
	) -> anyhow::Result<Self> {
		let mut w = Writer::new();
		events.try_for_each(|e| w.render_event(&e))?;

		let title = w
			.toc
			.first()
			.ok_or_else(|| anyhow!("No heading found to infer title from"))?;

		let last_modified_output = Command::new("git")
			.arg("log")
			.arg("-1")
			.arg(r#"--pretty=format:%cI"#)
			.arg(&path)
			.output()
			.with_context(|| anyhow!("Could not read the last modification date for file"))?
			.stdout;
		let last_modified = String::from_utf8(last_modified_output)?;
		let last_modified = DateTime::parse_from_rfc3339(&last_modified).with_context(|| {
			anyhow!(
				"Failed to parse datetime returned by git '{}'",
				last_modified
			)
		})?;

		Ok(Self {
			title: title.to_owned(),
			route: PageRoute::from_path(&path)?,
			config: w.config,
			toc: w.toc,
			word_count: w.word_count,
			last_modified,
			path,
		})
	}
}

#[derive(Debug, Clone)]
pub struct Heading {
	#[allow(dead_code)]
	pub level: u8,
	pub id: String,
	pub text: String,
	pub html: String,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum State {
	Toplevel,
	Heading,
	Config,
}

struct Writer<'s> {
	/// This renderer is used for generating the html for the titles
	html_renderer: html::Writer<'s>,
	config: PageConfig,
	toc: Vec<Heading>,
	toml_text: String,
	state: State,
	word_count: usize,
}

impl<'s> Writer<'s> {
	fn new() -> Self {
		Self {
			html_renderer: html::Writer::new(None),
			config: PageConfig::default(),
			toc: Vec::new(),
			toml_text: String::new(),
			state: State::Toplevel,
			word_count: 0,
		}
	}

	fn render_event(&mut self, e: &jotdown::Event<'s>) -> anyhow::Result<()> {
		if let Event::Str(content) = e {
			if self.state != State::Config {
				self.word_count += content
					.split(" ")
					.filter(|w| w.contains(|c: char| c.is_alphabetic()))
					.count()
			}
		}

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
						self.state = State::Config
					}
				}
			}
			Event::End(Container::RawBlock { format: "toml" }) => {
				if self.state == State::Config {
					self.state = State::Toplevel;

					let config: PageConfig = toml::from_str(&self.toml_text)
						.with_context(|| "Failed to parse page config in TOML format")?;
					self.config.merge(config)?;

					self.toml_text.clear();
				}
			}
			Event::Str(str) if self.state == State::Config => {
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
