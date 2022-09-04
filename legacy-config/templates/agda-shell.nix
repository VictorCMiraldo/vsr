{ pkgs ? import <nixpkgs> {} }:
let
  agdaWithStandardLibrary = pkgs.agda.withPackages (p: [ p.standard-library ]);
in
  pkgs.mkShell {
    buildInputs = [ agdaWithStandardLibrary ];
  }
