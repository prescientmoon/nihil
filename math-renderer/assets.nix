{
  fetchurl,
  stdenvNoCC,
  fetchzip,
  symlinkJoin,
  ...
}:

let
  stylesheet = stdenvNoCC.mkDerivation {
    pname = "pulldown-latex-stylesheet";
    version = "unstable-2025-08-13";

    src = fetchurl {
      url = "https://github.com/carloskiki/pulldown-latex/releases/download/v0.7.1/styles.css";
      sha256 = "1p6ki39x8cid6550vapw6cz3cngdmffmsskdlhinh7391l9a3g2q";
    };

    dontUnpack = true;

    installPhase = ''
      runHook preInstall
      mkdir -p $out/share/css/
      cp $src $out/share/css/styles.css
      runHook postInstall
    '';
  };

  fonts = stdenvNoCC.mkDerivation {
    pname = "pulldown-latex-fonts";
    version = "unstable-2025-08-13";

    src = fetchzip {
      url = "https://github.com/carloskiki/pulldown-latex/releases/download/v0.7.1/font.zip";
      sha256 = "sha256-RKUrFHvSPISPT0sBBeij87uhR3cG0LpnT9FE4ZeDxzs=";
    };

    installPhase = ''
      runHook preInstall
      mkdir -p $out/share/fonts/woff2
      cp -r $src/*.woff2 $out/share/fonts/woff2
      runHook postInstall
    '';
  };
in
symlinkJoin {
  name = "pulldown-latex-assets";
  paths = [
    stylesheet
    fonts
  ];
}
