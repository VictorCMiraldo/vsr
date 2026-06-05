{ config, lib, ... }:
{
  xdg.userDirs = lib.mkIf (!config.vsr.isServer) {
    enable = true;
    desktop = "$HOME/tmp/Desktop";
    documents = "$HOME/doc";
    download = "$HOME/tmp";
    templates = "$HOME/.templates";
    music = "$HOME/data/Music";
    videos = "$HOME/data/Videos";
    pictures = "$HOME/data/Pictures";
    createDirectories = true;
  };
}
