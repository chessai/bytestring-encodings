{ package ? "bytestring-encodings", compiler ? "ghc822" }:

(import ./default.nix {
  inherit package compiler;
}).bytestring-encodings
