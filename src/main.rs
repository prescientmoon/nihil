use std::path::PathBuf;
use std::str::FromStr;

use anyhow::Context;
use metadata::should_refresh_last_modified;

mod generate;
mod html;
mod metadata;
mod template;

fn main() -> anyhow::Result<()> {
	let public_path = PathBuf::from_str("public")?;
	let dist_path = PathBuf::from_str(
		std::env::var("MOONYTHM_OUT_DIR")
			.as_deref()
			.unwrap_or("dist"),
	)?;

	if dist_path.exists() {
		std::fs::remove_dir_all(&dist_path).with_context(|| "Cannot delete `dist` directory")?;
	}

	std::fs::create_dir(&dist_path).with_context(|| "Cannot create `dist` directory")?;

	let mut pages = generate::Pages::new(PathBuf::from_str("content").unwrap(), dist_path.clone())?;

	pages
		.add_dir(&PathBuf::from_str("")?)
		.with_context(|| "Failed to collect directories")?;
	pages
		.generate()
		.with_context(|| "Failed to generate markup")?;

	for p in std::fs::read_dir(public_path)? {
		generate::copy_recursively(&p?.path(), &dist_path)
			.with_context(|| "Cannot copy `public` -> `dist`")?;
	}

	if should_refresh_last_modified() {
		pages.last_modified_cache.save()?;
	}

	Ok(())
}
