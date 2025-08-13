{ pkgs }:
pkgs.mkShell {
  packages = with pkgs.haskell.packages."ghc9101"; [
    cabal-install
    ghcid
    fourmolu
    haskell-language-server
    (pkgs.callPackage ../highlighter { })
  ];

  NIHIL_MUTATE = 1;
  NIHIL_STATE = "./state.toml";
  NIHIL_BASE_URL = "http://localhost:8080";
  NIHIL_PUBLIC = "../public";
  NIHIL_CONTENT = "../content";
  NIHIL_OUT = "../dist";
  NIHIL_DRAFTS = 1;
  NIHIL_LMODERN =
    let
      lmodern = pkgs.callPackage ./lmodern.nix { };
    in
    "${lmodern}/share/fonts/woff2/public/";
  NIHIL_CMODERN = "${pkgs.cm_unicode}/share/fonts/opentype/";
}
