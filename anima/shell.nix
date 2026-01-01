let
  sources = import ../npins;
  pkgs = import sources.nixpkgs { };
  odin = import sources.odin { inherit pkgs; };
in
pkgs.mkShell rec {
  nativeBuildInputs = [
    odin
    pkgs.mold
    pkgs.just
    pkgs.ols
    pkgs.seer
  ];

  buildInputs = [
  ];

  LD_LIBRARY_PATH = with pkgs; lib.makeLibraryPath buildInputs;
}
