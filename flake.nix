{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    moonythm.url = "git+ssh://forgejo@ssh.git.moonythm.dev/prescientmoon/moonythm.git";
    moonythm.flake = false;
  };

  outputs =
    inputs@{ nixpkgs, ... }:
    {
      packages."x86_64-linux" =
        let
          pkgs = nixpkgs.legacyPackages."x86_64-linux";
        in
        rec {
          default = moonythm;
          moonythm = pkgs.callPackage ./. {
            inherit (inputs) moonythm;
            inherit nihil;
          };
          nihil = pkgs.callPackage ./nihil { };
          math-renderer = pkgs.callPackage ./math-renderer { };
          highlighter = pkgs.callPackage ./highlighter { };
        };

      devShells."x86_64-linux" =
        let
          pkgs = nixpkgs.legacyPackages."x86_64-linux";
        in
        rec {
          default = moonythm;
          moonythm = import ./shell.nix { inherit pkgs; };
          nihil = import ./nihil/shell.nix { inherit pkgs; };
          math-renderer = import ./math-renderer/shell.nix { inherit pkgs; };
          highlighter = import ./highlighter/shell.nix { inherit pkgs; };
        };
    };
}
