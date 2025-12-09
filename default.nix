let
  sources = import ./npins;
in
{
  pkgs ? import sources.nixpkgs { },
}:
pkgs.lib.fix (self: {
  nihil-math-renderer = pkgs.callPackage ./nihil-math-renderer { };
  nihil-math-assets = pkgs.callPackage ./nihil-math-renderer/assets.nix { };
  nihil-highlighter = pkgs.callPackage ./nihil-highlighter { };
  nihil = pkgs.callPackage ./nihil {
    inherit (self) nihil-math-renderer nihil-highlighter nihil-math-assets;
  };

  moonythm = pkgs.callPackage ./moonythm {
    inherit (sources) moonythm;
    inherit (self) nihil;
  };
})
