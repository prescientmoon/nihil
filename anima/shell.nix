let
  sources = import ../npins;
  pkgs = import sources.nixpkgs { };
in
pkgs.mkShell rec {
  nativeBuildInputs = [
    pkgs.odin
    pkgs.mold
    pkgs.just
    pkgs.ols
  ];

  buildInputs = [
  ];

  LD_LIBRARY_PATH = with pkgs; lib.makeLibraryPath buildInputs;
}
