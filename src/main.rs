mod html;
mod tex;
fn main() {
	let djot_input = std::fs::read_to_string("content/arcaea.dj").unwrap();
	let events = jotdown::Parser::new(&djot_input);
	let html = crate::html::render_to_string(events);
	println!("{html}");
}
