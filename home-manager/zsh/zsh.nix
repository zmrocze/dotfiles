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
      initExtra = builtins.readFile ./zshrc;
      # initExtraFirst
      # initExtraBeforeCompInit
      plugins = [{
        name = "powerlevel10k";
        file = "powerlevel10k.zsh-theme";
        src = inputs.powerlevel10k;
      }];
    };
  };
}
