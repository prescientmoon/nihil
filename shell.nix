{ pkgs }:
let
  nihil = pkgs.callPackage ./nihil { };
in
pkgs.mkShell {
  packages = [
    pkgs.http-server
    pkgs.nodejs
    pkgs.just
    pkgs.terser
    pkgs.libxml2
    pkgs.validator-nu
    pkgs.htmltest
    pkgs.watchexec
    pkgs.parallel
    nihil
  ];
}
