use std::fmt::Write;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::str::FromStr;

use crate::metadata::{LastModifiedCache, PageMetadata};
use crate::template;
use anyhow::{bail, Context};

pub fn copy_recursively(from: &Path, to: &Path) -> anyhow::Result<()> {
	Command::new("cp").arg("-r").arg(from).arg(to).output()?;

	Ok(())
}

pub struct Pages<'a> {
	pages: Vec<PageMetadata<'a>>,
	assets: Vec<PathBuf>,
	pub last_modified_cache: LastModifiedCache,

	content_root: PathBuf,
	out_root: PathBuf,
	base_url: String,
}

impl<'a> Pages<'a> {
	pub fn new(content_root: PathBuf, out_root: PathBuf) -> anyhow::Result<Self> {
		Ok(Self {
			last_modified_cache: LastModifiedCache::from_file()?,
			pages: Vec::new(),
			assets: Vec::new(),
			content_root,
			out_root,
			base_url: std::env::var("MOONYTHM_BASE_URL")
				.unwrap_or_else(|_| "http://localhost:8080".to_owned()),
		})
	}

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
		let metadata =
			PageMetadata::new(&mut self.last_modified_cache, content_path, source, events)?;

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
	pub fn generate(&self) -> anyhow::Result<()> {
		for page in &self.pages {
			let out_dir = self.out_root.join(page.route.to_path());
			std::fs::create_dir_all(&out_dir)
				.with_context(|| format!("Failed to generate {out_dir:?} directory"))?;

			let mut out = String::new();
			let mut page_renderer = template!("templates/page.html", &mut out)?;

			page_renderer.feed(&mut out, |label, out| {
				match label {
					"title" => {
						let mut w = crate::html::Writer::new(page, &[], &self.base_url);
						w.states.push(crate::html::State::TextOnly);

						for event in &page.title.events {
							w.render_event(event, out)?;
						}
					}
					"description" => {
						let mut w = crate::html::Writer::new(page, &[], &self.base_url);
						w.states.push(crate::html::State::TextOnly);

						for event in &page.description {
							w.render_event(event, out)?;
						}
					}
					"url" => {
						write!(
							out,
							"{}/{}",
							self.base_url,
							page.route.to_path().to_str().unwrap(),
						)?;
					}
					"content" => {
						let mut w = crate::html::Writer::new(page, &self.pages, &self.base_url);

						for event in jotdown::Parser::new(page.source) {
							w.render_event(&event, out)?;
						}

						w.render_epilogue(out)?;
					}
					_ => bail!("Unknown label {label} in page template"),
				}

				Ok(true)
			})?;

			std::fs::write(out_dir.join("index.html"), out)
				.with_context(|| format!("Failed to write {out_dir:?} post"))?;
		}

		for path in &self.assets {
			std::fs::create_dir_all(self.out_root.join(path).parent().unwrap())
				.with_context(|| format!("Failed to create parent dir for asset at {path:?}"))?;

			std::fs::copy(self.content_root.join(path), self.out_root.join(path))
				.with_context(|| format!("Failed to copy asset at {path:?}"))?;
		}

		self.generate_sitemap()?;

		Ok(())
	}
	// }}}
	// {{{ Generate sitemap
	fn generate_sitemap(&self) -> anyhow::Result<()> {
		let mut out = String::new();

		write!(
			out,
			r#"
        <?xml version="1.0" encoding="UTF-8" standalone="yes"?>
			  <urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">
      "#
		)?;

		for page in &self.pages {
			if page.config.sitemap_exclude || page.config.hidden {
				continue;
			}

			write!(
				out,
				"<url>
          <loc>{}/{}</loc>
          <lastmod>{}</lastmod>
        ",
				self.base_url,
				page.route.to_path().to_str().unwrap(),
				page.last_modified.to_rfc3339()
			)?;

			if let Some(priority) = page.config.sitemap_priority {
				write!(out, "<priority>{priority}</priority>")?;
			}

			if let Some(changefreq) = &page.config.sitemap_changefreq {
				write!(out, "<changefreq>{changefreq}</changefreq>")?;
			}

			write!(out, "</url>")?;
		}

		write!(out, "</urlset>")?;

		std::fs::write(self.out_root.join("sitemap.xml"), out.trim())
			.with_context(|| "Failed to write sitemap")?;

		Ok(())
	}
	// }}}
}
