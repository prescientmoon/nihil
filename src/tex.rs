use anyhow::anyhow;
use cached::{proc_macro::io_cached, DiskCache};

fn make_cache() -> DiskCache<&'static str, String> {
	DiskCache::new("tex-cache")
		.set_disk_directory("/home/moon/.cache/moonythm")
		.build()
		.unwrap()
}

#[io_cached(
	disk = true,
	map_error = r##"|err| anyhow::Error::from(err)"##,
	create = r"{ make_cache() }"
)]
pub fn compile_tex(string: &str) -> Result<String, anyhow::Error> {
	let res = std::process::Command::new("latexmlmath")
		.arg("--preload=amsfonts")
		.arg("--strict")
		.arg("--quiet")
		.arg(string)
		.output()
		.expect("failed to execute latexmathml process");

	if res.status.success() {
		let output = String::from_utf8_lossy(&res.stdout);
		Ok(output.trim().to_owned())
	} else {
		let output = String::from_utf8_lossy(&res.stderr);
		Err(anyhow!("{}", output.trim()))
	}
}
