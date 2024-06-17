{ pkgs, ... }:

{
  config.home.packages = with pkgs;
    let
      cli = let
        coreutils = [
          bat
          bash
          bottom
          efibootmgr
          eza
          fd
          fzf
          gnugrep
          gzip
          gnused
          gnutar
          gnupg
          htop
          inxi
          just
          less
          lsof
          lshw
          netcat-openbsd
          ncurses
          openssl
          procs
          pv
          rm-improved
          socat
          traceroute
          tree
          tldr
          unzip
          wget
          which
          xz
          yq
          jq
          zoxide
          zip
        ];
      in coreutils ++ [
        cpulimit
        cryptsetup # software encryption
        # dust
        flatpak
        f2fs-tools
        gcc13
        gdb
        bear
        cachix
        difftastic
        nixos-generators
        jupyter
        usbutils
        nix-melt
        nix-tree
        nix-prefetch-git
        pciutils
        micro
        monero-cli
        refind
        rustup
        sedutil # applied patch in sed.nix
        vim
        texlive.combined.scheme-full
      ];
      audio = [
        alsa-utils
        alsa-scarlett-gui # control focusrite scarlett audio interface
        bitwig-studio
        carla
        glava # visualizer
        helvum # graph view
        jackmix # mixer
        jackmeter # graph view
        reaper
        qjackctl # graph view
        qpwgraph # graph view
        vmpk # virtual midi piano keyboard
        vkeybd # virtual midi piano keyboard
        yabridge # vst bridge
        yabridgectl # vst bridge
      ];
      gui = let
        vm = [
          qemu
          gnome.gnome-boxes
          wineWowPackages.stableFull
          (lutris.override {
            extraLibraries = pkgs: with pkgs; [ openssl gnome.zenity ];
          })
          q4wine # qt-based gui for wine
        ];
      in vm ++ [
        aliza # mri scans
        amberol # music player
        drumkv1 # drum machine
        firefox
        gnome.dconf-editor
        gnome.gnome-terminal
        gimp
        gthumb
        hydrogen # drum machine
        gparted
        gnome.gnome-software
        kooha # screen recorder
        libreoffice-fresh
        obs-studio # screen recorder
        onlyoffice-bin
        obsidian
        pomodoro
        pinta # paint
        spotify
        tor
        tor-browser
        ungoogled-chromium
        vscode
        wpsoffice
        wireshark
      ];
      deps = [
        meslo-lgs-nf # font
        gmp # gnu multiple precision arithmetic library
        xclip
      ];
    in builtins.concatLists [ cli audio gui deps ];
}
