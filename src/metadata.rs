use std::ffi::OsStr;
use std::fmt::Write;
use std::path::{Component, Path, PathBuf};
use std::process::Command;
use std::str::FromStr;

use anyhow::{anyhow, bail, Context};
use chrono::{DateTime, FixedOffset, Utc};
use jotdown::{Attributes, Container, Event};
use serde::Deserialize;

pub fn should_refresh_last_modified() -> bool {
	std::env::var("MOONYTHM_UPDATE_LAST_MODIFIED").unwrap_or_default() == "1"
}

// {{{ Config
#[derive(Deserialize, Debug, Default)]
pub struct PageConfig {
	pub created_at: Option<DateTime<FixedOffset>>,
	pub sitemap_priority: Option<f32>,
	pub sitemap_changefreq: Option<String>,

	#[serde(default)]
	pub sitemap_exclude: bool,

	#[serde(default)]
	pub hidden: bool,

	#[serde(default)]
	pub draft: bool,

	/// Hides away the traditional header
	#[serde(default)]
	pub compact: bool,
}

impl PageConfig {
	// {{{ Merge a single property. Errors out on duplicate values.
	fn merge_prop<A: PartialEq + std::fmt::Debug>(
		label: &str,
		first: Option<A>,
		second: Option<A>,
	) -> anyhow::Result<Option<A>> {
		match first {
			None => Ok(second),
			Some(first) => {
				if let Some(second) = second {
					if second != first {
						bail!(
							"Conflicting values for `{label}` page attribute: {first:?} and {second:?}"
						);
					}
				}

				Ok(Some(first))
			}
		}
	}
	// }}}
	// {{{ Config merging
	/// Merge another config into the current one. Might error out on duplicate values.
	fn merge(&mut self, other: PageConfig) -> anyhow::Result<()> {
		self.created_at = Self::merge_prop("created_at", self.created_at, other.created_at)?;

		self.sitemap_priority = Self::merge_prop(
			"sitemap_priority",
			self.sitemap_priority,
			other.sitemap_priority,
		)?;

		self.sitemap_changefreq = Self::merge_prop(
			"sitemap_changefreq",
			self.sitemap_changefreq.take(),
			other.sitemap_changefreq,
		)?;

		self.sitemap_exclude |= other.sitemap_exclude;
		self.hidden |= other.hidden;
		self.draft |= other.draft;
		self.compact |= other.compact;

		Ok(())
	}
	// }}}
}
// }}}
// {{{ Routing
#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum PageRoute {
	Home,
	NotFound,
	Posts,
	Post(String),
}

impl PageRoute {
	// {{{ Convert a path to a route
	fn from_path(path: &Path) -> anyhow::Result<Self> {
		let Some(Component::Normal(first)) = path.components().nth(1) else {
			bail!("Path is too short");
		};

		let result = if first == OsStr::new("index.dj") {
			Self::Home
		} else if first == OsStr::new("404.dj") {
			Self::NotFound
		} else if first == OsStr::new("echoes") {
			let Some(Component::Normal(second)) = path.components().nth(2) else {
				bail!("Cannot convert path '{:?}' to echo route", path);
			};
			let mut slice = second.to_str().unwrap();
			if slice.ends_with(".dj") {
				slice = slice.strip_suffix(".dj").unwrap();
			}

			if slice == "index" {
				Self::Posts
			} else {
				Self::Post(slice.to_owned())
			}
		} else {
			bail!("Cannot convert path '{:?}' to page route", path);
		};

		Ok(result)
	}
	// }}}
	// {{{ Convert a route to a path
	#[inline]
	pub fn to_path(&self) -> PathBuf {
		match self {
			Self::Home => PathBuf::from_str("").unwrap(),
			Self::NotFound => PathBuf::from_str("404").unwrap(),
			Self::Posts => PathBuf::from_str("echoes").unwrap(),
			Self::Post(id) => PathBuf::from_str(&format!("echoes/{id}")).unwrap(),
		}
	}
	// }}}
}
// }}}
// {{{ Metadata
#[derive(Debug, Clone)]
pub struct Heading<'a> {
	#[allow(dead_code)]
	pub level: u8,
	pub id: String, // Heading events own their ID, so we have to clone
	pub events: Vec<jotdown::Event<'a>>,
}

#[derive(Debug)]
pub struct PageMetadata<'s> {
	pub config: PageConfig,
	pub route: PageRoute,
	pub source_path: PathBuf,

	pub title: Heading<'s>,
	pub description: Vec<jotdown::Event<'s>>,
	#[allow(dead_code)]
	pub toc: Vec<Heading<'s>>,
	pub source: &'s str,

	pub word_count: usize,
	pub last_modified: DateTime<FixedOffset>,
}

impl<'a> PageMetadata<'a> {
	pub fn new(
		last_modified_cache: &mut LastModifiedCache,
		path: PathBuf,
		source: &'a str,
		mut events: impl Iterator<Item = Event<'a>>,
	) -> anyhow::Result<Self> {
		let route = PageRoute::from_path(&path)?;
		let last_modified = if should_refresh_last_modified() {
			let last_modified_output = Command::new("git")
				.arg("log")
				.arg("-1")
				.arg(r#"--pretty=format:%cI"#)
				.arg(&path)
				.output()
				.with_context(|| anyhow!("Could not read the last modification date for file"))?
				.stdout;
			let last_modified = String::from_utf8(last_modified_output)?;

			let last_modified = if last_modified.is_empty() {
				Utc::now().fixed_offset()
			} else {
				DateTime::parse_from_rfc3339(&last_modified).with_context(|| {
					anyhow!(
						"Failed to parse datetime returned by git '{}'",
						last_modified
					)
				})?
			};

			last_modified_cache
				.pages
				.push((route.clone(), last_modified));

			last_modified
		} else {
			last_modified_cache
				.pages
				.iter()
				.find(|item| item.0 == route)
				.map(|(_, last_modified)| *last_modified)
				.unwrap_or_else(|| Utc::now().fixed_offset())
		};

		let mut w = Writer::new();
		events.try_for_each(|e| w.render_event(&e))?;

		let title = w
			.toc
			.first()
			.ok_or_else(|| anyhow!("No heading found to infer title from"))?;

		Ok(Self {
			route,
			title: title.clone(),
			last_modified,
			source_path: path,
			source,
			config: w.config,
			description: w.description,
			toc: w.toc,
			word_count: w.word_count,
		})
	}
}
// }}}
// {{{ Metadata parsing
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum State {
	Toplevel,
	Heading,
	Config,
	Description,
}

struct Writer<'s> {
	config: PageConfig,
	toc: Vec<Heading<'s>>,
	toml_text: String,
	state: State,
	word_count: usize,
	description: Vec<jotdown::Event<'s>>,
}

impl<'s> Writer<'s> {
	fn new() -> Self {
		Self {
			config: PageConfig::default(),
			description: Vec::new(),
			toc: Vec::new(),
			toml_text: String::new(),
			state: State::Toplevel,
			word_count: 0,
		}
	}

	fn render_event<'a>(&mut self, e: &'a jotdown::Event<'s>) -> anyhow::Result<()> {
		if let Event::Str(content) = e {
			if self.state != State::Config {
				self.word_count += content
					.split(" ")
					.filter(|w| w.contains(|c: char| c.is_alphabetic()))
					.count()
			}
		}

		match e {
			// {{{ Headings
			Event::Start(Container::Heading { level, id, .. }, _) => {
				assert_eq!(self.state, State::Toplevel);
				self.state = State::Heading;
				self.toc.push(Heading {
					level: *level as u8,
					events: Vec::new(),
					// These ids are always borrowed, unless modified by the user (i.e. me)
					id: id.to_string(),
				})
			}
			Event::End(Container::Heading { .. }) => {
				assert_eq!(self.state, State::Heading);
				self.state = State::Toplevel;
			}
			// }}}
			// {{{ TOML config blocks
			Event::Start(Container::RawBlock { format: "toml" }, attrs) => {
				assert_eq!(self.state, State::Toplevel);
				if has_role(attrs, "config") {
					self.state = State::Config
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
			// }}}
			// {{{ Descriptions
			Event::Start(Container::Div { .. }, attrs) if self.state == State::Toplevel => {
				if has_role(attrs, "description") {
					self.state = State::Description
				}
			}
			Event::End(Container::Div { .. }) if self.state == State::Description => {
				self.state = State::Toplevel;
			}
			// }}}
			Event::Str(str) if self.state == State::Config => {
				self.toml_text.write_str(str)?;
			}
			_ if self.state == State::Description => {
				self.description.push(e.clone());
			}
			_ if self.state == State::Heading => {
				let last_heading = self.toc.last_mut().unwrap();
				last_heading.events.push(e.clone());
			}
			_ => {}
		}

		Ok(())
	}
}
// }}}
// {{{ Helpers
pub fn has_role(attrs: &Attributes, value: &str) -> bool {
	attrs
		.get_value("role")
		.map_or(false, |role| format!("{role}") == value)
}
// }}}
// {{{ Last modified cache
#[derive(Debug, Clone, Default, serde::Serialize, serde::Deserialize)]
pub struct LastModifiedCache {
	pages: Vec<(PageRoute, DateTime<FixedOffset>)>,
}

impl LastModifiedCache {
	pub fn from_file() -> anyhow::Result<LastModifiedCache> {
		if should_refresh_last_modified() {
			Ok(Self::default())
		} else {
			Ok(toml::de::from_str(&std::fs::read_to_string(
				"last_modified.toml",
			)?)?)
		}
	}

	pub fn save(&self) -> anyhow::Result<()> {
		Ok(std::fs::write(
			"last_modified.toml",
			toml::ser::to_string(self)?,
		)?)
	}
}
// }}}
