{ pkgs }:
pkgs.mkShell {
  packages = with pkgs.haskell.packages."ghc9101"; [
    cabal-install
    ghcid
    fourmolu
    haskell-language-server
    (pkgs.callPackage ../highlighter { })
    (pkgs.callPackage ../math-renderer { })
  ];

  NIHIL_MUTATE = 1;
  NIHIL_STATE = "./state.toml";
  NIHIL_BASE_URL = "http://localhost:8080";
  NIHIL_CONTENT = "../content,../public";
  NIHIL_OUT = "../dist";
  NIHIL_DRAFTS = 1;
  NIHIL_MATH_ASSETS = pkgs.callPackage ../math-renderer/assets.nix { };
  NIHIL_CMODERN = "${pkgs.cm_unicode}/share/fonts/opentype/";
}
