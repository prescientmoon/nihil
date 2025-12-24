let
  sources = import ../npins;
  pkgs = import sources.nixpkgs { };
in
pkgs.mkShell rec {
  nativeBuildInputs = with pkgs.haskellPackages; [
    cabal-install
    ghcid
    fourmolu
    haskell-language-server
  ];

  buildInputs = [
    pkgs.zlib
    (pkgs.callPackage ../highlighter { })
    (pkgs.callPackage ../math-renderer { })
  ];

  LD_LIBRARY_PATH = with pkgs; lib.makeLibraryPath buildInputs;

  NIHIL_MUTATE = 1;
  NIHIL_BASE_URL = "http://localhost:8080";
  NIHIL_STATE = "../../moonythm/state.toml";
  NIHIL_CONTENT = "../../moonythm/content,../../moonythm/public,../public";
  NIHIL_OUT = "../dist";
  NIHIL_DRAFTS = 1;
  NIHIL_MATH_ASSETS = pkgs.callPackage ../math-renderer/assets.nix { };
}
