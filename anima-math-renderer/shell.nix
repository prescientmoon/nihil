let
  sources = import ../npins;
  pkgs = import sources.nixpkgs { };
in
pkgs.mkShell {
  packages = [
    pkgs.cargo
    pkgs.rustc
    pkgs.clippy
    pkgs.rust-analyzer
    pkgs.rustfmt
  ];
}
