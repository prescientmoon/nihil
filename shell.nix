{ pkgs }:
pkgs.mkShell {
  packages = [
    pkgs.http-server
    pkgs.nodejs
    pkgs.just
    pkgs.terser
    pkgs.libxml2
    pkgs.validator-nu
    pkgs.htmltest
  ];
}
