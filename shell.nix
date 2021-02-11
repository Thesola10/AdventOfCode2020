{ nixpkgs ? import <nixpkgs> {}}:

let
  inherit (nixpkgs) pkgs;
  inherit (pkgs) haskellPackages;
  
  haskellDeps = ps: with ps; [
    split
    regex-tdfa
  ];

  ghc = haskellPackages.ghcWithPackages haskellDeps;
in
pkgs.stdenv.mkDerivation {
  name = "haskell";
  buildInputs = [ ghc ];
}
