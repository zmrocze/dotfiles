{ inputs, config, ... }: {
  imports = [
    # paths to other modules
  ];

  options = {
    # option declarations
  };

  # note: homemanager zsh plugin is terrible set of string macros and undocumented conventions,
  # it doesn't provide lot of value, so don't hesitate rewriting it with simpler set of string macros.
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
        # in case you'd want a local directory history, when searching with UP key. saved to ./.directory_history
        # {
        #   name = "zsh-directory-history";
        #   file = "directory-history.plugin.zsh";
        #   src = inputs.zsh-directory-history;
        # }
        # {
        #   name = "zsh-autocomplete";
        #   file = "zsh-autocomplete.plugin.zsh";
        #   src = inputs.zsh-autocomplete;
        # }
      ];
      # sources plugin
      # enableSyntaxHighlighting = true;
      syntaxHighlighting.enable = true;
      # "autoload -U compinit && compinit" is already in zshrc
      completionInit = "";
    };

    # this turns out not to be picked by default, source instead
    # home.file = { ".p10k.zsh".source = ./p10k.zsh; };
  };
}
