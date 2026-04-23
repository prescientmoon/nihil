let
  sources = import ../npins;
in
{
  pkgs ? import sources.nixpkgs { },
}:
{
  anima-rust-utils = pkgs.callPackage (import ./anima-rust-utils.nix) { };
}
