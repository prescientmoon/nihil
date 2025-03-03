{
  lib,
  rustPlatform,
}:
rustPlatform.buildRustPackage {
  pname = "moonythm-generator";
  version = "unstable-2025-03-03";
  src = lib.fileset.toSource {
    root = ../.;
    fileset = lib.fileset.unions [
      ../Cargo.lock
      ../Cargo.toml
      ../src
    ];
  };

  nativeBuildInputs = [ ];
  buildInputs = [ ];

  useFetchCargoVendor = true;
  cargoLock = {
    lockFile = ../Cargo.lock;
    outputHashes = {
      "tree-sitter-djot-2.0.0" = "sha256-7qwBdueO33SdOp5KY12WMIkDgjS5Psz2eF804wn/aLk=";
    };
  };

  # Disable all tests
  doCheck = false;

  meta = {
    description = "Static site generator tailor-made for moonythm.dev";
    homepage = "https://git.moonythm.dev/prescientmoon/moonythm";
    mainProgram = "moonythm";
    platforms = [ "x86_64-linux" ];
  };
}
