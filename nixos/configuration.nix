# This is your system's configuration file.
# Use this to configure your system environment (it replaces /etc/nixos/configuration.nix)

{ inputs, lib, config, pkgs, username, hostname, ... }: {
  # You can import other NixOS modules here
  imports = [
    # If you want to use modules from other flakes (such as nixos-hardware):
    # inputs.hardware.nixosModules.common-cpu-amd
    # inputs.hardware.nixosModules.common-ssd

    # You can also split up your configuration and import pieces of it here:
    # ./users.nix

    # Import your generated (nixos-generate-config) hardware configuration
    ./hardware-configuration.nix
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
    };
  };

  nix = {
    # This will add each flake input as a registry
    # To make nix3 commands consistent with your flake
    registry = lib.mapAttrs (_: value: { flake = value; }) inputs;

    # This will additionally add your inputs to the system's legacy channels
    # Making legacy nix commands consistent as well, awesome!
    nixPath = lib.mapAttrsToList (key: value: "${key}=${value.to.path}")
      config.nix.registry;

    settings = {
      # Enable flakes and new 'nix' command
      experimental-features = "nix-command flakes";
      # Deduplicate and optimize nix store
      auto-optimise-store = true;
      keep-outputs = true;
      keep-derivations = true;
      # Binary Cache
      trusted-public-keys = [
        "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
        "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
        "iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo="
        "mlabs.cachix.org-1:gStKdEqNKcrlSQw5iMW6wFCj3+b+1ASpBVY2SYuNV2M="
      ];
      substituters = [
        "https://cache.iog.io"
        "https://cache.nixos.org"
        "https://iohk.cachix.org"
        "https://mlabs.cachix.org"
      ];
    };
  };

  # TODO: Set your hostname
  networking.hostName = "${hostname}";

  # be able to mount windows
  boot.supportedFilesystems = [ "ntfs" ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = false;
  # boot immedietely into latest generation. To bypass press shift while booting into systemd
  boot.loader.timeout = 0;
  # https://discourse.nixos.org/t/easy-refind-boot-by-booting-into-systemd-boot-from-refind/28507/5?u=zmrocze
  # boot.loader.efi.efiSysMountPoint = "/boot/efi";

  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Enable networking
  networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "Europe/Warsaw";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "pl_PL.UTF-8";
    LC_IDENTIFICATION = "pl_PL.UTF-8";
    LC_MEASUREMENT = "pl_PL.UTF-8";
    LC_MONETARY = "pl_PL.UTF-8";
    LC_NAME = "pl_PL.UTF-8";
    LC_NUMERIC = "pl_PL.UTF-8";
    LC_PAPER = "pl_PL.UTF-8";
    LC_TELEPHONE = "pl_PL.UTF-8";
    LC_TIME = "pl_PL.UTF-8";
  };

  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;

    # Enable the GNOME Desktop Environment.
    displayManager.gdm.enable = true;
    desktopManager.gnome.enable = true;
    # Configure keymap in X11
    layout = "pl";
    # TODO: what is this?
    xkbVariant = "";
  };

  environment.gnome.excludePackages = (with pkgs;
    [
      # gnome-photos
      # gnome-tour
      gnome-console
    ]) ++ (with pkgs.gnome; [
      # cheese # webcam tool
      # gnome-music
      gedit # text editor
      epiphany # web browser
      geary # email reader
      # gnome-characters
      tali # poker game
      iagno # go game
      hitori # sudoku game
      atomix # puzzle game
      yelp # Help view
      gnome-contacts
      # gnome-initial-setup
    ]);
  environment.etc."xdg/user-dirs.defaults".source = etc/user-dirs.defaults;

  console.keyMap = "pl2";
  services.printing.enable = true;
  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  services = {
    syncthing = {
      enable = true;
      user = "${username}";
      dataDir =
        "/home/${username}/Shared"; # Default folder for new synced folders
      configDir =
        "/home/${username}/Shared/.config/syncthing"; # Folder for Syncthing's settings and keys
    };
  };

  # TODO: Configure your system-wide user settings (groups, etc), add more users as needed.
  users.users = {
    # FIXME: Replace with your username
    "${username}" = {
      # TODO: You can set an initial password for your user.
      # If you do, you can skip setting a root password by passing '--no-root-passwd' to nixos-install.
      # Be sure to change it (using passwd) after rebooting!
      initialPassword = "correcthorsebatterystaple";
      isNormalUser = true;
      openssh.authorizedKeys.keys = [
        # TODO: Add your SSH public key(s) here, if you plan on using SSH to connect
      ];
      # TODO: Be sure to add any other groups you need (such as networkmanager, audio, docker, etc)
      extraGroups = [ "wheel" "audio" "networkmanager" ];
      shell = pkgs.zsh;
    };
  };
  # this enabled `$ man alias`
  environment.systemPackages = [ pkgs.man-pages pkgs.man-pages-posix ];
  # this no clue what it does
  documentation.dev.enable = true;

  # somehow this is needed here as well as home.nix,
  # https://www.reddit.com/r/NixOS/comments/z16mt8/cant_seem_to_set_default_shell_using_homemanager/
  programs.zsh.enable = true;
  # no effect, overwritten by sth
  # environment.etc."static/zshrc".source = lib.mkForce etc/zshrc;

  programs.dconf.enable = true;

  # This setups a SSH server. Very important if you're setting up a headless system.
  # Feel free to remove if you don't need it.
  services.openssh = {
    enable = false;
    # Forbid root login through SSH.
    # permitRootLogin = "no";
    # Use keys only. Remove if you want to SSH using password (not recommended)
    # passwordAuthentication = false;
  };

  # services.flatpak.enable = true;
  # services.accounts-daemon.enable = true;

  # for spotify
  networking.firewall.enable = true;
  networking.firewall.allowedTCPPorts = [ 57621 ];

  virtualisation.docker = {
    enable = true;
    rootless = {
      enable = true;
      setSocketVariable = true;
    };
  };

  hardware.bluetooth.enable = true;

  # security.doas.enable = true;
  # security.sudo.enable = false;
  # security.doas.extraConfig = builtins.readFile ./doas.conf;

  #   # Enable sound with pipewire.
  sound.enable = true;
  security.rtkit.enable = true;
  hardware.pulseaudio.enable = false;
  # security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # If you want to use JACK applications, uncomment this
    #jack.enable = true;

    # use the example session manager (no others are packaged yet so this is enabled by default,
    # no need to redefine it in your config for now)
    #media-session.enable = true;
  };

  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  system.stateVersion = "22.11";
}
