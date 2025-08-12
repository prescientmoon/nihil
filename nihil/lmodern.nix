{
  stdenvNoCC,
  fetchurl,
  woff2,
}:

stdenvNoCC.mkDerivation rec {
  pname = "lmodern";
  version = "2.005";

  src = fetchurl {
    url = "mirror://debian/pool/main/l/${pname}/${pname}_${version}.orig.tar.gz";
    hash = "sha256-xlUuZt6rjW0pX4t6PKWAHkkv3PisGCj7ZwatZPAUNxk=";
  };

  nativeBuildInputs = [ woff2 ];
  installPhase = ''
    runHook preInstall

    mkdir -p $out/share/fonts/

    cp -r fonts/{opentype,type1} $out/share/fonts/

    runHook postInstall
  '';

  # Convert to woff2
  buildPhase = ''
    runHook preBuild

    shopt -s globstar
    mkdir -p $out/share/fonts/woff2
    cd fonts/opentype
    for font in ./**/*.otf; do
      mkdir -p "$(dirname "$out/share/fonts/woff2/$font")"
      cp "$font" "$out/share/fonts/woff2/$font"
      woff2_compress "$out/share/fonts/woff2/$font"
    done
    cd ../../
    rm -rf $out/share/fonts/woff2/**/*.otf

    runHook postBuild
  '';
}
