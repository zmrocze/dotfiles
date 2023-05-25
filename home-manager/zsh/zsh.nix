{ inputs, config, ... }: {
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
      initExtra = builtins.readFile ./zshrc + builtins.readFile ./functions.zsh;
      # initExtraFirst
      # initExtraBeforeCompInit
      plugins = [{
        name = "powerlevel10k";
        file = "powerlevel10k.zsh-theme";
        src = inputs.powerlevel10k;
      }];
      # sources plugin
      enableSyntaxHighlighting = true;
      historySubstringSearch.enable = true;
      # "autoload -U compinit && compinit" is already in zshrc
      completionInit = "";
    };

    home.file = { ".p10k.zsh".source = ./p10k.zsh; };
  };
}
