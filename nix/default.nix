{ ... }:
{
  perSystem =
    { config, final, ... }:
    {
      packages = {
        moonythm-generator = final.callPackage (import ./moonythm-generator.nix) { };
        moonythm = final.callPackage (import ./moonythm.nix) { };
      };

      overlayAttrs = {
        inherit (config.packages) moonythm-generator moonythm;
      };
    };
}
