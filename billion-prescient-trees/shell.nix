let
  sources = import ../npins;
  pkgs = import sources.nixpkgs { };
  odin = pkgs.odin.overrideAttrs (og: {
    version = "unstable-2026-03-23";
    src = pkgs.fetchFromGitHub {
      fetchLFS = true;
      owner = "odin-lang";
      repo = "Odin";
      rev = "d90cc4e3b6a8647f81b70464c983cc8093154c62";
      sha256 = "sha256-fGNU/TD+I10dnMcIT06OfcwsrX7B8JvttK5ybUCi2ds=";
    };
    patches = [ (builtins.elem 0 og.patches) ]; # The second patch is broken
  });
in
pkgs.mkShell rec {
  nativeBuildInputs = [
    odin
    pkgs.mold
    pkgs.just
    pkgs.seer
    pkgs.valgrind
    pkgs.samply
  ];

  buildInputs = [ ];
  LD_LIBRARY_PATH = with pkgs; lib.makeLibraryPath buildInputs;
}
