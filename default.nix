{ compiler ? "ghc822" }:

with rec {
  pkgs = (import ./nix/nixpkgs.nix {
    inherit compiler; 
  });
  drv = pkgs.haskellPackages.bytestring-encodings;
};

drv
