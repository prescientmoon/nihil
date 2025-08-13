use std::{fmt::Display, io::Read};

use tree_sitter::Language;
use tree_sitter_highlight::{Highlight, HighlightConfiguration, HighlightEvent, Highlighter};

fn main() {
	let args: Vec<String> = std::env::args().collect();
	if args.len() < 2 {
		eprintln!("Usage: moonythm-highlighter <language>");
		std::process::exit(1);
	}

	let lang: &str = &args[1];

	let mut input = String::new();
	println!("got here");
	std::io::stdin()
		.read_to_string(&mut input)
		.expect("Failed to read from stdin");

	let grammar = match lang {
		"rust" => Some((
			"rust",
			Language::new(tree_sitter_rust::LANGUAGE),
			tree_sitter_rust::HIGHLIGHTS_QUERY,
			tree_sitter_rust::INJECTIONS_QUERY,
			"",
		)),
		"djot" => Some((
			"dj",
			tree_sitter_djot::language(),
			tree_sitter_djot::HIGHLIGHTS_QUERY,
			tree_sitter_djot::INJECTIONS_QUERY,
			"",
		)),
		"html" => Some((
			"html",
			Language::new(tree_sitter_html::LANGUAGE),
			tree_sitter_html::HIGHLIGHTS_QUERY,
			tree_sitter_html::INJECTIONS_QUERY,
			"",
		)),
		"ts" => Some((
			"typescript",
			Language::new(tree_sitter_typescript::LANGUAGE_TYPESCRIPT),
			concat!(
				include_str!(concat!(
					env!("NVIM_TREESITTER"),
					"/queries/ecma/highlights.scm"
				)),
				"\n",
				include_str!(concat!(
					env!("NVIM_TREESITTER"),
					"/queries/typescript/highlights.scm"
				))
			),
			"",
			tree_sitter_typescript::LOCALS_QUERY,
		)),
		_ => None,
	};

	if let Some((ft, ts_language, highlights, injections, locals)) = grammar {
		let mut highlighter = Highlighter::new();

		let mut config =
			HighlightConfiguration::new(ts_language, ft, highlights, injections, locals)
				.expect("Failed to create language config");

		let highlight_names = [
			"attribute",
			"comment",
			"comment.documentation",
			"constant",
			"constant.builtin",
			"constructor",
			"function",
			"function.declaration",
			"function.builtin",
			"function.macro",
			"function.method",
			"keyword",
			"label",
			"operator",
			"property",
			"punctuation",
			"punctuation.bracket",
			"punctuation.delimiter",
			"string",
			"string.special",
			"tag",
			"type",
			"type.builtin",
			"variable",
			"variable.builtin",
			"variable.parameter",
		];

		let highlight_classes = highlight_names
			.iter()
			.map(|s| s.replace(".", "-"))
			.collect::<Vec<_>>();

		config.configure(&highlight_names);

		let highlights = highlighter
			.highlight(&config, input.as_bytes(), None, |_| None)
			.expect("Highlighter failed");

		for event in highlights {
			match event {
				Ok(HighlightEvent::Source { start, end }) => {
					print!("{}", Escaped(&input[start..end]));
				}
				Ok(HighlightEvent::HighlightStart(Highlight(index))) => {
					print!(r#"<span class="{}">"#, highlight_classes[index]);
				}
				Ok(HighlightEvent::HighlightEnd) => {
					print!(r#"</span>"#);
				}
				_ => continue,
			}
		}
	} else {
		print!("{input}");
	}
}

// {{{ HTMl escaper
pub struct Escaped<'a>(&'a str);

impl Display for Escaped<'_> {
	fn fmt(&self, out: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		let mut s = self.0;
		let mut ent = "";
		while let Some(i) = s.find(|c| {
			match c {
				'<' => Some("&lt;"),
				'>' => Some("&gt;"),
				'&' => Some("&amp;"),
				'"' => Some("&quot;"),
				_ => None,
			}
			.is_some_and(|s| {
				ent = s;
				true
			})
		}) {
			out.write_str(&s[..i])?;
			out.write_str(ent)?;
			s = &s[i + 1..];
		}
		out.write_str(s)
	}
}
// }}}
