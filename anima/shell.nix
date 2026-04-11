let
  sources = import ../npins;
  pkgs = import sources.nixpkgs { };
  all = import ../. { inherit pkgs; };
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

  # ols = (pkgs.ols.override { inherit odin; }).overrideAttrs {
  #   version = "unstable-2026-02-20";
  #   src = pkgs.fetchFromGitHub {
  #     owner = "DanielGavin";
  #     repo = "ols";
  #     rev = "28ce44287b495f26cd1e5c01332a51e52e294fe2";
  #     sha256 = "0p5hr0zc667pqks0p5dm3p8w5ry08vai4i14v2pmnj26a82vh8j3";
  #   };
  # };

in
pkgs.mkShell rec {
  nativeBuildInputs = [
    odin
    pkgs.mold
    pkgs.just
    pkgs.seer
    pkgs.valgrind
    pkgs.live-server
    pkgs.watchexec
    pkgs.parallel
  ];

  buildInputs = [
    all.anima-math-renderer
  ];

  LD_LIBRARY_PATH = with pkgs; lib.makeLibraryPath buildInputs;
  ANIMA_MATH_ASSETS = all.anima-math-assets;
}
