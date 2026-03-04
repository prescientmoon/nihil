let
  sources = import ../npins;
  pkgs = import sources.nixpkgs { };
  odin = pkgs.odin.overrideAttrs (og: {
    version = "unstable-2026-03-04";
    src = pkgs.fetchFromGitHub {
      fetchLFS = true;
      owner = "odin-lang";
      repo = "Odin";
      rev = "217b4967b0769c3d9580012e3c4c923ab6b1b64d";
      sha256 = "sha256-eUXbRPwr/3VOv1UGoYkU9qVaHGZ04Nu1uSiUAtpN8ss=";
    };
    patches = [ (builtins.elem 0 og.patches) ]; # The second patch is broken
  });
  #
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
    # pkgs.odin
    # ols
    pkgs.mold
    pkgs.just
    pkgs.seer
    pkgs.valgrind
  ];

  buildInputs = [
  ];

  LD_LIBRARY_PATH = with pkgs; lib.makeLibraryPath buildInputs;
}
