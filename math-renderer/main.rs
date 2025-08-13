use std::io::Read;

fn main() {
	let args: Vec<String> = std::env::args().collect();
	if args.len() != 2 {
		eprintln!("Usage: moonythm-math-renderer <inline|block>");
		std::process::exit(1);
	}

	let mut input = String::new();
	std::io::stdin()
		.read_to_string(&mut input)
		.expect("Failed to read from stdin");

	let config = pulldown_latex::RenderConfig {
		display_mode: {
			use pulldown_latex::config::DisplayMode::*;
			match args[1].as_ref() {
				"block" => Block,
				"inline" => Inline,
				other => panic!("Unknown display mode {other}"),
			}
		},
		annotation: None,
		error_color: (178, 34, 34),
		xml: true,
		math_style: pulldown_latex::config::MathStyle::TeX,
	};

	let mut mathml = String::new();
	let storage = pulldown_latex::Storage::new();
	let parser = pulldown_latex::Parser::new(&input, &storage);
	pulldown_latex::push_mathml(&mut mathml, parser, config).unwrap();
	print!("{mathml}");
}
