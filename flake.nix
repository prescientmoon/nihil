{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  inputs.flake-parts.url = "github:hercules-ci/flake-parts";

  outputs =
    inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" ];
      perSystem =
        { pkgs, lib, ... }:
        {
          devShells.default = pkgs.mkShell rec {
            nativeBuildInputs = with pkgs; [
              pkgs.cargo
              pkgs.rustc
              pkgs.clippy
              pkgs.rust-analyzer
              pkgs.rustfmt
              pkgs.imagemagick
            ];

            buildInputs = with pkgs; [ ];

            LD_LIBRARY_PATH = lib.makeLibraryPath buildInputs;
          };
        };
    };
}
