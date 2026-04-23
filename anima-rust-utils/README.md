This directory exposes Rust-based utilities I can call from Odin. I do this in order to make use of Rust's ecosystem. I currently expose the following functionality:

- LaTeX rendering — there's many generic solutions, but I really like the output of the `pullback_latex` crate
- Image dimension detection — the Odin standard library can do this already, but does not currently support the `WebP` format.
