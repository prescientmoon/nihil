use std::path::{Path, PathBuf};
use std::process::Command;
use std::str::FromStr;

use anyhow::Context;
use html::Renderer;
use template::TemplateRenderer;

mod html;
mod template;
mod tex;

fn copy_recursively(from: &Path, to: &Path) -> anyhow::Result<()> {
	Command::new("cp").arg("-r").arg(from).arg(to).output()?;

	Ok(())
}

fn main() -> anyhow::Result<()> {
	let dist_path = PathBuf::from_str("dist")?;
	let public_path = PathBuf::from_str("public")?;

	if dist_path.exists() {
		std::fs::remove_dir_all(&dist_path).with_context(|| "Cannot delete `dist` directory")?;
	}

	std::fs::create_dir(&dist_path).with_context(|| "Cannot create `dist` directory")?;

	for p in std::fs::read_dir(public_path)? {
		copy_recursively(&p?.path(), &dist_path)
			.with_context(|| "Cannot copy `public` -> `dist`")?;
	}

	// {{{ Generate contents
	let djot_input = std::fs::read_to_string("content/arcaea.dj").unwrap();
	let mut out = String::new();
	let page_template = template!("templates/page.html")?;
	let mut page_renderer = TemplateRenderer::new(&page_template);

	while let Some(label) = page_renderer.next(&mut out)? {
		if label == "content" {
			let events = jotdown::Parser::new(&djot_input);
			let html = Renderer::new()?;
			html.push(events, &mut out)?;
		} else {
			break;
		}
	}

	page_renderer.finish(&mut out)?;
	// }}}

	let posts_dir = dist_path.join("posts");
	std::fs::create_dir(&posts_dir).with_context(|| "Cannot create `dist/posts` directory")?;

	std::fs::write(posts_dir.join("arcaea.html"), out)
		.with_context(|| "Failed to write `arcaea.html` post")?;

	Ok(())
}
