{ pkgs }:
pkgs.mkShell {
  packages = with pkgs.haskell.packages."ghc9101"; [
    cabal-install
    ghcid
    fourmolu
    haskell-language-server
  ];

  NIHIL_MUTATE = 1;
  NIHIL_STATE = "./state.toml";
  NIHIL_BASE_URL = "http://localhost:8080";
  NIHIL_PUBLIC = "../public";
  NIHIL_CONTENT = "../content";
  NIHIL_OUT = "../dist";
  NIHIL_DRAFTS = 1;
  NIHIL_LMODERN_WEB =
    let
      lmodern = pkgs.callPackage ./lmodern.nix { };
    in
    "${lmodern}/share/fonts";
}
