{
  lib,
  symlinkJoin,
  haskellPackages,
  makeWrapper,
  cm_unicode,
  nihil-highlighter,
  nihil-math-renderer,
  nihil-math-assets,
}:
symlinkJoin {
  name = "nihil";
  paths = [ (haskellPackages.callPackage ./nihil.nix { }) ];

  buildInputs = [ makeWrapper ];
  postBuild = ''
    wrapProgram $out/bin/nihil \
      --prefix PATH : ${
        lib.makeBinPath [
          nihil-highlighter
          nihil-math-renderer
        ]
      } \
      --set NIHIL_MATH_ASSETS "${nihil-math-assets}" \
      --set NIHIL_CMODERN "${cm_unicode}/share/fonts/opentype/"
  '';
}
