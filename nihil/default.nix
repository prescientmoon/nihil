{
  lib,
  callPackage,
  symlinkJoin,
  haskellPackages,
  makeWrapper,
  cm_unicode,
}:
symlinkJoin {
  name = "nihil";
  paths = [ (haskellPackages.callPackage ./nihil.nix { }) ];

  buildInputs = [ makeWrapper ];
  postBuild = ''
    wrapProgram $out/bin/nihil \
      --prefix PATH : ${
        lib.makeBinPath [
          (callPackage ../highlighter { })
          (callPackage ../math-renderer { })
        ]
      } \
      --set NIHIL_MATH_ASSETS "${callPackage ../math-renderer/assets.nix { }}" \
      --set NIHIL_CMODERN "${cm_unicode}/share/fonts/opentype/"
  '';
}
