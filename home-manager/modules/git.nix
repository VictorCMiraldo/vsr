{
  config,
  pkgs,
  lib,
  ...
}:
{
  config = {
    programs.git = {
      enable = true;
      settings = {
        user = {
          name = "Victor Miraldo";
          email =
            if config.vsr.isWorkMachine then "victor.miraldo@fastmail.com" else "victor.miraldo@converge.io";
        };
        alias = {
          please = "push --force-with-lease";
          graph = "log --graph --oneline --decorate --all";
        };
        fetch.prune = true;
        push.autoSetupRemote = true;
        init.defaultBranch = "main";
        merge.conflictstyle = "zdiff3";
        diff.algorithm = "histogram";
      };
    };
  };
}
