# This is your home-manager configuration file
# Use this to configure your home environment (it replaces ~/.config/nixpkgs/home.nix)
{ config, inputs, username, ... }: {
  # You can import other home-manager modules here
  imports =
    [ ./packages.nix zsh/zsh.nix gnome/gnome.nix inputs.nur.nixosModules.nur ];

  home = {
    # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
    stateVersion = "22.11";
    inherit username;
    homeDirectory = "/home/${username}";
  };

  # home.file = { ".config/user-dirs.dirs".source = ./config/user-dirs.dirs; };

  # Add stuff for your user as you see fit:
  # programs.neovim.enable = true;

  # Enable home-manager and git
  programs = {
    home-manager.enable = true;
    git = {
      enable = true;
      userEmail = "karolochmanmilarski@gmail.com";
      userName = "zmrocze";
      difftastic.enable = true;
      extraConfig = {
        init.defaultBranch = "main";
        core.editor = "micro";
        credential = {
          credentialStore = "secretservice";
          helper =
            "${config.nur.repos.utybo.git-credential-manager}/bin/git-credential-manager";
        };
      };
    };
    direnv = {
      enable = true;
      enableZshIntegration = true;
      nix-direnv.enable = true;
    };
    # dconf.enable = true;
  };

  # https://discourse.nixos.org/t/home-manager-nerdfonts/11226
  fonts.fontconfig.enable = true;

  # Nicely reload system units when changing configs
  systemd.user.startServices = "sd-switch";
}
