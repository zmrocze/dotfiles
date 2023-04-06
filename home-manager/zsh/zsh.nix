{ inputs, lib, config, pkgs, ... }: {
  imports = [
    # paths to other modules
  ];

  options = {
    # option declarations
  };

  config = {
    # option definitions
    programs.zsh = {
      enable = true;
      initExtra = lib.readfile ./.zshrc
      # initExtraFirst
      # initExtraBeforeCompInit
    }
  };
}