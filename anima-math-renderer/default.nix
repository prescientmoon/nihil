let
  sources = import ../npins;
in
{
  pkgs ? import sources.nixpkgs { },
}:
{
  anima-math-renderer = pkgs.callPackage (import ./anima-math-renderer.nix) { };
  anima-math-assets = pkgs.callPackage (import ./assets.nix) { };
}
