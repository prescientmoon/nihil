{
  mkDerivation,
  base,
  directory,
  djot,
  file-embed,
  filepath,
  lib,
  process,
  relude,
  time,
  tomland,
  moonythm-highlighter,
  moonythm-math-renderer,
}:
mkDerivation {
  pname = "nihil";
  version = "0.1.0.0";
  src = lib.fileset.toSource {
    root = ./.;
    fileset = lib.fileset.unions [
      ./nihil.cabal
      ./src
      ./templates
    ];
  };

  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base
    directory
    djot
    file-embed
    filepath
    process
    relude
    time
    tomland
    moonythm-highlighter
    moonythm-math-renderer
  ];

  license = lib.licenses.agpl3Plus;
  mainProgram = "nihil";
}
