{
  lib,
  rustPlatform,
  vimPlugins,
}:
rustPlatform.buildRustPackage {
  pname = "moonythm-highlighter";
  version = "unstable-2025-08-13";
  src = lib.fileset.toSource {
    root = ./.;
    fileset = lib.fileset.unions [
      ./Cargo.lock
      ./Cargo.toml
      ./main.rs
    ];
  };

  nativeBuildInputs = [ ];
  buildInputs = [ ];

  NVIM_TREESITTER = vimPlugins.nvim-treesitter.withAllGrammars;

  useFetchCargoVendor = true;
  cargoLock = {
    lockFile = ./Cargo.lock;
    outputHashes = {
      "tree-sitter-djot-2.0.0" = "sha256-7qwBdueO33SdOp5KY12WMIkDgjS5Psz2eF804wn/aLk=";
    };
  };

  # Disable all tests
  doCheck = false;

  meta = {
    description = "Treesitter based syntax highlighter for https://moonythm.dev.";
    mainProgram = "moonythm-highlighter";
    platforms = [ "x86_64-linux" ];
  };
}
