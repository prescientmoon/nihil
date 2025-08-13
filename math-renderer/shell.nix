{ pkgs }:
pkgs.mkShell {
  packages = [
    # Rust tooling
    pkgs.cargo
    pkgs.rustc
    pkgs.clippy
    pkgs.rust-analyzer
    pkgs.rustfmt
  ];
}
