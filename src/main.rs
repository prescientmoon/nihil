use std::fmt::Write;
use std::fs::{self};
use std::path::{Path, PathBuf};
use std::process::Command;
use std::str::FromStr;

use anyhow::{anyhow, bail, Context};
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
fn generate_page<'s>(
	path: &Path,
	pages: Option<&[PageMetadata]>,
) -> anyhow::Result<PageMetadata<'s>> {
	let content_path = PathBuf::from_str("content")?.join(path);

	let djot_input = std::fs::read_to_string(&content_path).unwrap();

	// We leak all the file contents, which is fine, as we expect them to
	// live on for the rest of the duration of the program.
	//
	// I'm doing this because a lot of places want to reference this,
	// which makes memory management a bit nasty (I need to be able to
	// return the metadata out of this function, but that's not really allowed
	// if it's referencing a "local variable" like the initial string)
	let djot_input = Box::leak(Box::new(djot_input));
	let mut out = String::new();

	let mut page_renderer = template!("templates/page.html", &mut out)?;

	let events = jotdown::Parser::new(djot_input);
	let metadata = PageMetadata::new(content_path, events)?;

	while let Some(label) = page_renderer.current() {
		if label == "content" {
			let events = jotdown::Parser::new(djot_input);
			render_html(&metadata, pages, events, &mut out)?;
		} else if label == "navigation" {
			out.write_str(r#"<a href="/"><code>~</code></a>"#)?;
			out.write_str(" / ")?;
			out.write_str(r#"<a href="/echoes"><code>echoes</code></a>"#)?;
		} else {
			bail!("Unknown label {label} in page template")
		}

		page_renderer.next(&mut out)?;
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

	Ok(metadata)
}
// }}}
// {{{ Generate an entire directory
fn generate_dir(path: &Path) -> anyhow::Result<()> {
	let content_path = PathBuf::from_str("content")?.join(path);
	let out_path = PathBuf::from_str("dist")?.join(path);
	fs::create_dir_all(&out_path)
		.with_context(|| format!("Could not generate directory {path:?}"))?;
	let mut files = fs::read_dir(&content_path)?.collect::<Result<Vec<_>, _>>()?;

	// Iterates over the files, removing the `index.dj` file if it does exist.
	let has_index = files
		.iter()
		.enumerate()
		.find_map(|(i, f)| {
			if f.path().file_name().and_then(|f| f.to_str()) == Some("index.dj") {
				Some(i)
			} else {
				None
			}
		})
		.map(|index| files.swap_remove(index))
		.is_some();

	let mut pages = Vec::new();

	for file in files {
		let file_path = file.path();
		let filename = file_path.file_name().unwrap();
		let path = path.join(filename);
		if file_path.is_dir() {
			generate_dir(&path)?;
		} else if file_path.extension().map_or(false, |ext| ext == "dj") {
			pages.push(generate_page(&path, None)?);
		} else {
			fs::copy(content_path.join(filename), out_path.join(filename))?;
		}
	}

	if has_index {
		pages.sort_by_key(|post| (post.config.created_at, post.last_modified));

		let path = path.join("index.dj");
		generate_page(&path, Some(&pages))?;
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
