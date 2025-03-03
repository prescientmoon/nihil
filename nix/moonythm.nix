{
  moonythm-generator,
  runCommand,
}:
runCommand "moonythm" { } ''
  mkdir $out

  cd ${../.}
  MOONYTHM_OUT_DIR="$out" ${moonythm-generator}/bin/moonythm
''
