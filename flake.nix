{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  inputs.flake-parts.url = "github:hercules-ci/flake-parts";

  outputs =
    inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" ];
      imports = [
        inputs.flake-parts.flakeModules.easyOverlay
        ./nix
      ];

      perSystem =
        { pkgs, lib, ... }:
        {
          devShells.default = pkgs.mkShell rec {
            nativeBuildInputs = with pkgs; [
              # Rust tooling
              pkgs.cargo
              pkgs.rustc
              pkgs.clippy
              pkgs.rust-analyzer
              pkgs.rustfmt

              # Dev tooling
              pkgs.imagemagick
              pkgs.http-server

              # Build / test pipeline
              pkgs.nodejs
              pkgs.just
              pkgs.terser
              pkgs.libxml2
              pkgs.validator-nu
              pkgs.htmltest
            ];

            buildInputs = with pkgs; [ ];

            LD_LIBRARY_PATH = lib.makeLibraryPath buildInputs;
          };
        };
    };
}
