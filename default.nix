{
  lib,
  nihil,
  runCommand,
  moonythm,
}:
runCommand "moonythm"
  {
    NIHIL_BASE_URL = "https://moonythm.dev";
    NIHIL_STATE = "${moonythm.outPath}/state.toml";
    NIHIL_CONTENT = lib.concatStringsSep "," [
      "${moonythm.outPath}/content/"
      "${moonythm.outPath}/public/"
      "${./public}"
    ];
  }
  ''
    NIHIL_OUT="$out" ${nihil}/bin/nihil
  ''
