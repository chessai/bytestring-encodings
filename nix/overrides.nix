{ pkgs }:

self: super:

with { inherit (pkgs.stdenv) lib; };

with pkgs.haskell.lib;

{
  bytestring-encodings = (
    with rec {
      bytestring-encodingsSource = pkgs.lib.cleanSource ../.;
      bytestring-encodingsBasic = self.callCabal2nix "bytestring-encodings" bytestring-encodingsSource {};
    };
    overrideCabal bytestring-encodingsBasic (old: {
    })
  );
}
