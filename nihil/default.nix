{ pkgs, haskell }:
haskell.packages."ghc9101".callPackage ./nihil.nix {
  moonythm-highlighter = pkgs.callPackage ../highlighter { };
  moonythm-math-renderer = pkgs.callPackage ../math-renderer { };
}
