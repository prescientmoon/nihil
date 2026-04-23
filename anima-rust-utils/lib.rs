// {{{ Odin communication helpers
#[repr(C)]
#[derive(Debug)]
pub struct OdinStringView<'a> {
	pub ptr: *const u8,
	pub len: usize,
	// I want this to work with stable Rust, therefore I cannot use
	// #![feature(phantom_variance_markers)]
	pub _marker: std::marker::PhantomData<&'a str>,
}

#[repr(C)]
#[derive(Debug)]
pub struct OdinStringViewMut<'a> {
	pub ptr: *mut u8,
	pub len: usize,
	// I want this to work with stable Rust, therefore I cannot use
	// #![feature(phantom_variance_markers)]
	pub _marker: std::marker::PhantomData<&'a mut str>,
}

pub fn from_odin_string<'a>(view: OdinStringView<'a>) -> &'a str {
	let slice = unsafe { std::slice::from_raw_parts(view.ptr, view.len) };
	str::from_utf8(slice).expect("Invalid UTF-8")
}
// }}}

// {{{ LaTeX rendering
pub mod latex {
	use super::*;
	use std::slice;

	#[repr(u8)]
	#[derive(Debug)]
	pub enum MathMode {
		LaTeXInline = 1,
		LaTeXBlock = 2,
	}

	#[repr(C)]
	#[derive(Debug)]
	pub struct RenderMathInput<'a> {
		pub input: OdinStringView<'a>,
		pub output: *mut OdinStringViewMut<'a>,
		pub mode: MathMode,
	}

	#[repr(C)]
	pub struct RenderMathOutput {
		pub required_size: usize, // When 0 => we have enough memory
	}

	/// Will return a required size greater than 0 if the given buffer is not large enough.
	#[no_mangle]
	#[allow(clippy::missing_safety_doc)]
	pub unsafe extern "C" fn render_math(args: RenderMathInput) -> RenderMathOutput {
		let input_str = from_odin_string(args.input);

		let config = pulldown_latex::RenderConfig {
			display_mode: {
				use pulldown_latex::config::DisplayMode::*;
				match args.mode {
					MathMode::LaTeXBlock => Block,
					MathMode::LaTeXInline => Inline,
				}
			},
			annotation: None,
			error_color: (178, 34, 34),
			xml: true,
			math_style: pulldown_latex::config::MathStyle::TeX,
		};

		let mut mathml = String::new();
		let storage = pulldown_latex::Storage::new();
		let parser = pulldown_latex::Parser::new(input_str, &storage);
		pulldown_latex::push_mathml(&mut mathml, parser, config).unwrap();

		let output = args.output.as_mut().expect("Given null pointer to output");
		let bytes = mathml.as_bytes();
		if bytes.len() > output.len {
			return RenderMathOutput {
				required_size: bytes.len(),
			};
		}

		let output_slice = slice::from_raw_parts_mut(output.ptr, output.len);
		output_slice[..bytes.len()].copy_from_slice(&bytes[..bytes.len()]);
		output.len = bytes.len();

		RenderMathOutput { required_size: 0 }
	}
}
// }}}
// {{{ Image helpers
pub mod image {
	use super::*;

	#[repr(C)]
	#[derive(Debug)]
	pub struct ImageDimensions {
		pub width: usize,
		pub height: usize,
	}

	#[no_mangle]
	pub extern "C" fn image_dimensions(path: OdinStringView) -> ImageDimensions {
		let path = from_odin_string(path);
		let (w, h) = ::image::image_dimensions(path).expect("Invalid image");
		ImageDimensions {
			width: w as usize,
			height: h as usize,
		}
	}
}
// }}}
