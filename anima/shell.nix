let
  sources = import ../npins;
  pkgs = import sources.nixpkgs { };
  odin = pkgs.odin.overrideAttrs {
    version = "unstable-2026-02-28";
    src = pkgs.fetchFromGitHub {
      owner = "odin-lang";
      repo = "Odin";
      rev = "a0b9d710f72e114efdd4de7b76fa3ccc0182a90c";
      sha256 = "0kyg9wkn9g8rir7gxhi74ghryf3n9dv4lj27f5ngvxkw9v8r1q5r";
    };
  };

  ols = (pkgs.ols.override { inherit odin; }).overrideAttrs {
    version = "unstable-2026-02-28";
    src = pkgs.fetchFromGitHub {
      owner = "DanielGavin";
      repo = "ols";
      rev = "28ce44287b495f26cd1e5c01332a51e52e294fe2";
      sha256 = "0p5hr0zc667pqks0p5dm3p8w5ry08vai4i14v2pmnj26a82vh8j3";
    };
  };
in
pkgs.mkShell rec {
  nativeBuildInputs = [
    # pkgs.odin
    odin
    ols
    pkgs.mold
    pkgs.just
    pkgs.seer
    pkgs.valgrind
  ];

  buildInputs = [
  ];

  LD_LIBRARY_PATH = with pkgs; lib.makeLibraryPath buildInputs;
}
