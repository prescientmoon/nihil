use std::{fmt::Display, io::Read};

use tree_sitter::Language;
use tree_sitter_highlight::{Highlight, HighlightConfiguration, HighlightEvent, Highlighter};

static HIGHLIGHT_NAMES: [&str; 40] = [
	"attribute",
	"boolean",
	"comment",
	"comment.documentation",
	"constant",
	"constant.builtin",
	"constructor",
	"function",
	"function.builtin",
	"function.declaration",
	"function.macro",
	"function.method",
	"keyword",
	"keyword.repeat",
	"label",
	"markup",
	"markup.heading",
	"markup.italic",
	"markup.link",
	"markup.link.label",
	"markup.link.url",
	"markup.quote",
	"markup.raw",
	"markup.raw.block",
	"number",
	"operator",
	"property",
	"punctuation",
	"punctuation.bracket",
	"punctuation.delimiter",
	"string",
	"string.special",
	"spell",
	"tag",
	"type",
	"type.djot",
	"type.builtin",
	"variable",
	"variable.builtin",
	"variable.parameter",
];

fn main() {
	let args: Vec<String> = std::env::args().collect();
	if args.len() != 2 {
		eprintln!("Usage: nihil-highlighter <language>");
		std::process::exit(1);
	}

	let mut input = String::new();
	std::io::stdin()
		.read_to_string(&mut input)
		.expect("Failed to read from stdin");

	let lang: &str = &args[1];

	if let Some(config) = highlighter_config(lang) {
		let mut highlighter = Highlighter::new();

		let highlights = highlighter
			.highlight(&config, input.as_bytes(), None, |lang| {
				Box::leak(Box::new(highlighter_config(lang))).as_ref()
			})
			.expect("Highlighter failed");

		let highlight_classes = HIGHLIGHT_NAMES
			.iter()
			.map(|s| s.replace(".", "-"))
			.collect::<Vec<_>>();

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
		print!("{}", Escaped(&input));
	}
}

fn highlighter_config(lang: &str) -> Option<HighlightConfiguration> {
	let grammar = match lang {
		"rust" | "rs" => Some((
			"rust",
			Language::new(tree_sitter_rust::LANGUAGE),
			tree_sitter_rust::HIGHLIGHTS_QUERY,
			tree_sitter_rust::INJECTIONS_QUERY,
			"",
		)),
		// "haskell" | "hs" => Some((
		// 	"haskell",
		// 	Language::new(tree_sitter_haskell::LANGUAGE),
		// 	// I love nedless allocations + leaking memory aaaaaaaaaaaa (sorry)
		// 	// Wish concat! would work with any strings, not just literals...
		// 	concat!(
		// 		include_str!(concat!(
		// 			env!("NVIM_TREESITTER"),
		// 			"/queries/haskell/highlights.scm"
		// 		)),
		// 		r#"
		//         [ "∀" ] @keyword.repeat
		//       "#
		// 	),
		// 	include_str!(concat!(
		// 		env!("NVIM_TREESITTER"),
		// 		"/queries/haskell/injections.scm"
		// 	)),
		// 	include_str!(concat!(
		// 		env!("NVIM_TREESITTER"),
		// 		"/queries/haskell/locals.scm"
		// 	)),
		// )),
		"djot" | "dj" => Some((
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
		"toml" => Some((
			"toml",
			Language::new(tree_sitter_toml_ng::LANGUAGE),
			tree_sitter_toml_ng::HIGHLIGHTS_QUERY,
			"",
			"",
		)),
		// "typescript" | "ts" => Some((
		// 	"typescript",
		// 	Language::new(tree_sitter_typescript::LANGUAGE_TYPESCRIPT),
		// 	concat!(
		// 		include_str!(concat!(
		// 			env!("NVIM_TREESITTER"),
		// 			"/queries/ecma/highlights.scm"
		// 		)),
		// 		"\n",
		// 		include_str!(concat!(
		// 			env!("NVIM_TREESITTER"),
		// 			"/queries/typescript/highlights.scm"
		// 		))
		// 	),
		// 	"",
		// 	tree_sitter_typescript::LOCALS_QUERY,
		// )),
		// "typst" | "typ" => Some((
		// 	"typst",
		// 	tree_sitter_typst::language(),
		// 	include_str!(concat!(
		// 		env!("NVIM_TREESITTER"),
		// 		"/queries/typst/highlights.scm"
		// 	)),
		// 	include_str!(concat!(
		// 		env!("NVIM_TREESITTER"),
		// 		"/queries/typst/injections.scm"
		// 	)),
		// 	"",
		// )),
		// "agda" => Some((
		// 	"agda",
		// 	Language::new(tree_sitter_agda::LANGUAGE),
		// 	include_str!(concat!(
		// 		env!("NVIM_TREESITTER"),
		// 		"/queries/agda/highlights.scm"
		// 	)),
		// 	include_str!(concat!(
		// 		env!("NVIM_TREESITTER"),
		// 		"/queries/agda/injections.scm"
		// 	)),
		// 	"",
		// )),
		_ => None,
	}?;

	let (ft, ts_language, highlights, injections, locals) = grammar;
	let mut config = HighlightConfiguration::new(ts_language, ft, highlights, injections, locals)
		.expect("Failed to create language config");

	config.configure(&HIGHLIGHT_NAMES);

	Some(config)
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
