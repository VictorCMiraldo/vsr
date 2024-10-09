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
    # unison
    pandoc

    haskell.compiler.ghc964
    haskellPackages.cabal-install
    haskellPackages.ormolu
    haskellPackages.hpack
    haskellPackages.lhs2tex

    python311Packages.jedi-language-server

    agdaWithStdlib

    typescript
    nodePackages_latest.typescript-language-server

    ripgrep
  ];
}
