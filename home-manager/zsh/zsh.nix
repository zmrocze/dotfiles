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
      # + builtins.readFile ./p10k.zsh;
      # initExtraFirst
      # initExtraBeforeCompInit
      plugins = [
        {
          name = "powerlevel10k";
          file = "powerlevel10k.zsh-theme";
          src = inputs.powerlevel10k;
        }
        {
          name = "powerlevel10k-settings";
          file = "p10k.zsh";
          src = ./.;
        }
      ];
      # sources plugin
      enableSyntaxHighlighting = true;
      historySubstringSearch.enable = true;
      # "autoload -U compinit && compinit" is already in zshrc
      completionInit = "";
    };

    # this turns out not to be picked by default, source instead
    # home.file = { ".p10k.zsh".source = ./p10k.zsh; };
  };
}
