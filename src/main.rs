use std::fmt::Write;
use std::fs::{self};
use std::path::{Path, PathBuf};
use std::process::Command;
use std::str::FromStr;

use anyhow::{anyhow, Context};
use html::render_html;
use metadata::PageMetadata;

mod html;
mod metadata;
mod template;

fn copy_recursively(from: &Path, to: &Path) -> anyhow::Result<()> {
	Command::new("cp").arg("-r").arg(from).arg(to).output()?;

	Ok(())
}

// {{{ Generate single page
fn generate_page(path: &Path) -> anyhow::Result<()> {
	let content_path = PathBuf::from_str("content")?.join(path);

	let djot_input = std::fs::read_to_string(&content_path).unwrap();
	let mut out = String::new();

	let mut page_renderer = template!("templates/page.html", &mut out)?;

	let events = jotdown::Parser::new(&djot_input);
	let metadata = PageMetadata::new(events, content_path)?;

	while let Some(label) = page_renderer.current() {
		if label == "content" {
			let events = jotdown::Parser::new(&djot_input);
			render_html(&metadata, events, &mut out)?;
			page_renderer.next(&mut out)?;
		} else if label == "navigation" {
			out.write_str(r#"<a href="/"><code>~</code></a>"#)?;
			out.write_str(" / ")?;
			out.write_str(r#"<a href="/posts"><code>posts</code></a>"#)?;
			page_renderer.next(&mut out)?;
		} else {
			break;
		}
	}

	page_renderer.finish(&mut out)?;

	let mut out_path = PathBuf::from_str("dist")?.join(path);
	out_path.set_file_name(format!(
		"{}.html",
		path.file_stem()
			.ok_or_else(|| anyhow!("Empty filestem encountered"))?
			.to_str()
			.unwrap()
	));
	std::fs::write(out_path, out).with_context(|| "Failed to write `arcaea.html` post")?;

	Ok(())
}
// }}}
// {{{ Generate an entire directory
fn generate_dir(path: &Path) -> anyhow::Result<()> {
	let content_path = PathBuf::from_str("content")?.join(path);
	let out_path = PathBuf::from_str("dist")?.join(path);
	fs::create_dir_all(&out_path)
		.with_context(|| format!("Could not generate directory {path:?}"))?;

	for file in fs::read_dir(&content_path)? {
		let file_path = file?.path();
		let filename = file_path.file_name().unwrap();
		let path = path.join(filename);
		if file_path.is_dir() {
			generate_dir(&path)?;
		} else if file_path.extension().map_or(false, |ext| ext == "dj") {
			generate_page(&path)?;
		} else {
			fs::copy(content_path.join(filename), out_path.join(filename))?;
		}
	}

	Ok(())
}
// }}}

fn main() -> anyhow::Result<()> {
	let public_path = PathBuf::from_str("public")?;
	let dist_path = PathBuf::from_str("dist")?;

	if dist_path.exists() {
		std::fs::remove_dir_all(&dist_path).with_context(|| "Cannot delete `dist` directory")?;
	}

	std::fs::create_dir(&dist_path).with_context(|| "Cannot create `dist` directory")?;

	for p in std::fs::read_dir(public_path)? {
		copy_recursively(&p?.path(), &dist_path)
			.with_context(|| "Cannot copy `public` -> `dist`")?;
	}

	generate_dir(&PathBuf::new())?;

	Ok(())
}
