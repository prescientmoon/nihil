{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  outputs =
    { nixpkgs, ... }:
    {
      packages."x86_64-linux" =
        let
          pkgs = nixpkgs.legacyPackages."x86_64-linux";
        in
        {
          nihil = pkgs.callPackage ./nihil { };
          math-renderer = pkgs.callPackage ./math-renderer { };
          highlighter = pkgs.callPackage ./highlighter { };
        };

      devShells."x86_64-linux" =
        let
          pkgs = nixpkgs.legacyPackages."x86_64-linux";
        in
        {
          nihil = import ./nihil/shell.nix { inherit pkgs; };
          math-renderer = import ./math-renderer/shell.nix { inherit pkgs; };
          highlighter = import ./highlighter/shell.nix { inherit pkgs; };
        };
    };
}
