{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    stack
    haskellPackages.ghcide
    haskellPackages.haskell-language-server
    sass
  ];
}
