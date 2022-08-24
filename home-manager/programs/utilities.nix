{pkgs, ...}:
let
  agdaWithStdlib = pkgs.agda.withPackages (p: [ p.standard-library ]);

  texlive = pkgs.texlive.combine {
    inherit (pkgs.texlive)
     scheme-medium
     luatex
     latexmk
     polytable
     lazylist
     cleveref
      ;
  };
in {
  home.packages = with pkgs; [
    unison
    arandr
    pandoc

    haskell.compiler.ghc902
    haskellPackages.cabal-install
    haskellPackages.ormolu
    haskellPackages.hpack
    haskellPackages.lhs2tex

    agdaWithStdlib
  ];
}

