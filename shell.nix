{ package ? "is-utf8-bytestring", compiler ? "ghc822" }:

(import ./default.nix {
  inherit package compiler;
}).is-utf8-bytestring
