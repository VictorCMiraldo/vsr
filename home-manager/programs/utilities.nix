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
    konsave

    pandoc

    pyright
    basedpyright

    agdaWithStdlib

    typescript
    nodePackages_latest.typescript-language-server

    ripgrep

    # Formr youtube-dl
    yt-dlp
  ];
}
