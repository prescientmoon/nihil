use std::path::PathBuf;
use std::str::FromStr;

use anyhow::Context;

mod generate;
mod html;
mod metadata;
mod template;

fn main() -> anyhow::Result<()> {
	let public_path = PathBuf::from_str("public")?;
	let dist_path = PathBuf::from_str("dist")?;

	if dist_path.exists() {
		std::fs::remove_dir_all(&dist_path).with_context(|| "Cannot delete `dist` directory")?;
	}

	std::fs::create_dir(&dist_path).with_context(|| "Cannot create `dist` directory")?;

	let mut page = generate::Pages::new(
		PathBuf::from_str("content").unwrap(),
		PathBuf::from_str("dist").unwrap(),
	);

	page.add_dir(&PathBuf::from_str("")?)
		.with_context(|| "Failed to collect directories")?;
	page.generate()
		.with_context(|| "Failed to generate markup")?;

	for p in std::fs::read_dir(public_path)? {
		generate::copy_recursively(&p?.path(), &dist_path)
			.with_context(|| "Cannot copy `public` -> `dist`")?;
	}

	Ok(())
}
