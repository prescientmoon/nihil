{ pkgs }:
pkgs.mkShell {
  packages = [
    pkgs.just
  ];
}
