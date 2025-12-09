{
  lib,
  nihil,
  runCommand,
  moonythm,
}:
runCommand "moonythm"
  {
    NIHIL_BASE_URL = "https://moonythm.dev";
    NIHIL_STATE = "${moonythm}/state.toml";
    NIHIL_CONTENT = lib.concatStringsSep "," [
      "${moonythm}/content/"
      "${moonythm}/public/"
      "${./src}"
    ];
  }
  ''
    NIHIL_OUT="$out" ${nihil}/bin/nihil
  ''
