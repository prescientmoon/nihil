{ stdenvNoCC, fetchFromGitHub }:
stdenvNoCC.mkDerivation {
  pname = "lmodern-web";
  version = "unstable-2017-08-13";

  src = fetchFromGitHub {
    owner = "lalten";
    repo = "lmweb";
    rev = "ac7e4566999a87e7ad6de088a7fb269b71a9c847";
    sha256 = "1gmw9b13sjsk1xzvr8srdk39q856hahd6l2d433kprrkymwnp9j5";
  };

  installPhase = ''
    runHook preInstall

    mkdir -p $out/share/fonts/{woff,truetype}

    cp $src/font/*.woff $out/share/fonts/woff
    cp $src/font/*.ttf $out/share/fonts/truetype

    runHook postInstall
  '';
}
