{
  lib,
  rustPlatform,
  vimPlugins,
}:
rustPlatform.buildRustPackage {
  pname = "nihil-highlighter";
  version = "unstable-2025-08-13";
  src = lib.fileset.toSource {
    root = ./.;
    fileset = lib.fileset.unions [
      ./Cargo.lock
      ./Cargo.toml
      ./main.rs
    ];
  };

  NVIM_TREESITTER = vimPlugins.nvim-treesitter.withAllGrammars;

  cargoLock = {
    lockFile = ./Cargo.lock;
    outputHashes = {
      "tree-sitter-djot-2.0.0" = "sha256-7qwBdueO33SdOp5KY12WMIkDgjS5Psz2eF804wn/aLk=";
      "tree-sitter-typst-0.0.1" = "sha256-y2yw2ho5rmgwbom2S3EZy8J5nLbbwqivh+p0de84XDY=";
    };
  };

  meta = {
    description = "Treesitter based syntax highlighter for https://moonythm.dev.";
    mainProgram = "nihil-highlighter";
    platforms = [ "x86_64-linux" ];
  };
}
