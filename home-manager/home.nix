# This is your home-manager configuration file
# Use this to configure your home environment (it replaces ~/.config/nixpkgs/home.nix)

{ inputs, config, pkgs, username, ... }: {
  # You can import other home-manager modules here
  imports = [
    # If you want to use home-manager modules from other flakes (such as nix-colors):
    # inputs.nix-colors.homeManagerModule

    # You can also split up your configuration and import pieces of it here:
    # ./nvim.nix
    zsh/zsh.nix
    gnome/gnome.nix
    inputs.nur.nixosModules.nur
  ];

  nixpkgs = {
    # You can add overlays here
    overlays = [
      # If you want to use overlays exported from other flakes:
      # neovim-nightly-overlay.overlays.default

      # Or define it inline, for example:
      # (final: prev: {
      #   hi = final.hello.overrideAttrs (oldAttrs: {
      #     patches = [ ./change-hello-to-hi.patch ];
      #   });
      # })
    ];
    # Configure your nixpkgs instance
    config = {
      # Disable if you don't want unfree packages
      allowUnfree = true;
      # Workaround for https://github.com/nix-community/home-manager/issues/2942
      allowUnfreePredicate = _: true;
    };
  };

  home = {
    inherit username;
    homeDirectory = "/home/${username}";
  };

  # Add stuff for your user as you see fit:
  # programs.neovim.enable = true;
  home.packages = with pkgs; [
    meslo-lgs-nf
    bash
    bear
    haskellPackages.cabal-plan
    vscode
    # dconf-editor
    # direnv
    # docker
    # efibootmgr
    exa
    firefox
    # gnome-gedit
    # gimp
    glava
    gparted
    gnugrep
    gzip
    htop
    inxi
    jupyter
    less
    lsof
    # man-pages
    # micro
    monero-cli
    # npm
    # opam
    netcat-openbsd
    # opendoas
    openssl
    refind
    spotify
    gnused
    gnutar
    # texinfo
    # texlive-bibtexextra
    tldr
    tor
    # torsocks
    traceroute
    tree
    vim
    wget
    which
    xz
    yq
    jq
  ];

  # home.file = {

  # }

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
        # core.editor = pkgs.micro;
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
    };
  };

  # https://discourse.nixos.org/t/home-manager-nerdfonts/11226
  fonts.fontconfig.enable = true;

  # Nicely reload system units when changing configs
  systemd.user.startServices = "sd-switch";

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  home.stateVersion = "22.11";
}
