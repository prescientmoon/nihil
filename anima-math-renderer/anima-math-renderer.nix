{
  lib,
  rustPlatform,
}:
rustPlatform.buildRustPackage {
  pname = "anima-math-renderer";
  version = "unstable-2026-04-10";
  src = lib.fileset.toSource {
    root = ./.;
    fileset = lib.fileset.unions [
      ./Cargo.lock
      ./Cargo.toml
      ./lib.rs
    ];
  };

  cargoLock.lockFile = ./Cargo.lock;

  meta = {
    description = "LaTeX renderer for https://moonythm.dev.";
    platforms = [ "x86_64-linux" ];
  };
}
