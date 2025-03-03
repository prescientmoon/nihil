{
  moonythm-generator,
  runCommand,
}:
runCommand "moonythm" { } ''
  mkdir $out

  cd ${../.}

  export MOONYTHM_BASE_URL="https://moonythm.dev"
  MOONYTHM_OUT_DIR="$out" ${moonythm-generator}/bin/moonythm
''
