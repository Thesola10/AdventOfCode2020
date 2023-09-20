{ pkgs ? import <nixpkgs> {}}:

let
  inherit (pkgs) haskellPackages;
  
  haskellDeps = ps: with ps; [
    split
    regex-tdfa
  ];

  ghc = haskellPackages.ghcWithPackages haskellDeps;

  pkgs2 = import pkgs.path {
    overlays = [ (se: su: { inherit ghc; }) ];
  };
in
pkgs.stdenv.mkDerivation {
  name = "haskell";
  buildInputs = [ ghc pkgs2.haskell-language-server ];
}
