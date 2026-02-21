let
  sources = import ../npins;
  pkgs = import sources.nixpkgs { };
  ols = pkgs.ols.overrideAttrs {
    version = "unstable-2026-02-20";
    src = pkgs.fetchFromGitHub {
      owner = "DanielGavin";
      repo = "ols";
      rev = "dev-2026-02";
      sha256 = "sha256-3UoVMQuUol7vfSM57mj644XZ1CKmTz7+VuDSETT9NSE=";
    };
  };
in
pkgs.mkShell rec {
  nativeBuildInputs = [
    pkgs.odin
    ols
    pkgs.mold
    pkgs.just
    pkgs.seer
    pkgs.valgrind
  ];

  buildInputs = [
  ];

  LD_LIBRARY_PATH = with pkgs; lib.makeLibraryPath buildInputs;
}
