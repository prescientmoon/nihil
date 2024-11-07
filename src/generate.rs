use std::path::{Path, PathBuf};
use std::process::Command;
use std::str::FromStr;

use crate::html::render_html;
use crate::metadata::PageMetadata;
use crate::template;
use anyhow::{bail, Context};

pub fn copy_recursively(from: &Path, to: &Path) -> anyhow::Result<()> {
	Command::new("cp").arg("-r").arg(from).arg(to).output()?;

	Ok(())
}

#[derive(Debug, Default)]
pub struct Pages<'a> {
	pages: Vec<PageMetadata<'a>>,
	assets: Vec<PathBuf>,
}

impl<'a> Pages<'a> {
	// {{{ Collect simple pages
	pub fn add_page(&mut self, path: &Path) -> anyhow::Result<()> {
		let content_path = PathBuf::from_str("content")?.join(path);
		let source = std::fs::read_to_string(&content_path).unwrap();

		// We leak all the file contents, which is fine, as we expect them to
		// live on for the rest of the duration of the program.
		//
		// I'm doing this because a lot of places want to reference this,
		// which makes memory management a bit nasty (I need to be able to
		// return the metadata out of this function, but that's not really allowed
		// if it's referencing a "local variable" like the initial string)
		let source = Box::leak(Box::new(source));

		let events = jotdown::Parser::new(source);
		let metadata = PageMetadata::new(content_path, source, events)?;

		self.pages.push(metadata);

		Ok(())
	}
	// }}}
	// {{{ Collect directory of pages
	pub fn add_dir(&mut self, path: &Path) -> anyhow::Result<()> {
		let content_path = PathBuf::from_str("content")?.join(path);

		for file in std::fs::read_dir(&content_path)? {
			let file_path = file?.path();
			let filename = file_path.file_name().unwrap();
			let path = path.join(filename);
			if file_path.is_dir() {
				self.add_dir(&path)?;
			} else if file_path.extension().map_or(false, |ext| ext == "dj") {
				self.add_page(&path)?;
			} else {
				self.assets.push(path);
			}
		}

		Ok(())
	}
	// }}}
	// {{{ Generate
	pub fn generate(&self, out_root: PathBuf) -> anyhow::Result<()> {
		for page in &self.pages {
			let out_dir = out_root.join(page.route.to_path());
			std::fs::create_dir_all(&out_dir)
				.with_context(|| format!("Failed to generate {out_dir:?} directory"))?;

			let mut out = String::new();
			let mut page_renderer = template!("templates/page.html", &mut out)?;

			page_renderer.feed(&mut out, |label, out| {
				match label {
					"content" => {
						let events = jotdown::Parser::new(page.source);
						render_html(page, &self.pages, events, out)?;
					}
					_ => bail!("Unknown label {label} in page template"),
				}

				Ok(true)
			})?;

			std::fs::write(out_dir.join("index.html"), out)
				.with_context(|| format!("Failed to write {out_dir:?} post"))?;
		}

		let content_root = PathBuf::from_str("content")?;
		for path in &self.assets {
			std::fs::create_dir_all(out_root.join(path).parent().unwrap())
				.with_context(|| format!("Failed to create parent dir for asset at {path:?}"))?;

			std::fs::copy(content_root.join(path), out_root.join(path))
				.with_context(|| format!("Failed to copy asset at {path:?}"))?;
		}

		Ok(())
	}
	// }}}
}
