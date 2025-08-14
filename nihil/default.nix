{
  lib,
  callPackage,
  symlinkJoin,
  haskell,
  makeWrapper,
  cm_unicode,
}:
let
  base = haskell.packages."ghc9101".callPackage ./nihil.nix { };
in
symlinkJoin {
  name = "nihil";
  paths = [ base ];

  buildInputs = [
    makeWrapper
  ];

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
