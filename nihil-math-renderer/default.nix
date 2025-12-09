{
  lib,
  rustPlatform,
}:
rustPlatform.buildRustPackage {
  pname = "nihil-math-renderer";
  version = "unstable-2025-08-13";
  src = lib.fileset.toSource {
    root = ./.;
    fileset = lib.fileset.unions [
      ./Cargo.lock
      ./Cargo.toml
      ./main.rs
    ];
  };

  cargoLock.lockFile = ./Cargo.lock;

  meta = {
    description = "LaTeX renderer for https://moonythm.dev.";
    mainProgram = "nihil-math-renderer";
    platforms = [ "x86_64-linux" ];
  };
}
