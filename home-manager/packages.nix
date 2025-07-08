{ pkgs, ... }:

{
  config.home.packages = with pkgs;
    let
      cli = let
        coreutils = [
          bat
          bash
          bottom
          dust
          dutree
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
          puredata
          plugdata
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
        devenv
        flatpak
        f2fs-tools
        gcc13
        gdb
        bear
        cachix
        difftastic
        graphviz
        nixos-generators
        jupyter
        usbutils
        uv # python pip
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
        virtualenv
        (let
          tex = pkgs.texlive.combine {
            inherit (pkgs.texlive) scheme-full luatex;
            # dvisvgm dvipng # for preview and export as html
            # wrapfig amsmath ulem hyperref capt-of;
          };
        in tex)
      ];
      audio = [
        a2jmidid # alsa to jack midi
        alsa-utils
        alsa-scarlett-gui # control focusrite scarlett audio interface
        amberol # music player
        bitwig-studio
        carla
        drumkv1 # drum machine
        glava # visualizer
        guitarix # guitar amp
        gxplugins-lv2 # guitarix lv2 plugins
        hydrogen # drum machine
        helvum # graph view
        jackmix # mixer
        jackmeter # graph view
        mixxx # dj
        neural-amp-modeler-lv2 # guitar amp
        patchage # graph view
        paulstretch # time stretcher
        reaper
        surge # wavetable synth https://surge-synthesizer.github.io/
        qjackctl # graph view
        qpwgraph # graph view
        vmpk # virtual midi piano keyboard
        vkeybd # virtual midi piano keyboard
        vital # wavetable synth
        yabridge # vst bridge
        yabridgectl # vst bridge
      ];
      gui = let
        vm = [
          qemu
          gnome-boxes
          wineWowPackages.stableFull
          (lutris.override {
            extraLibraries = pkgs: with pkgs; [ openssl zenity ];
          })
          # q4wine # qt-based gui for wine
        ];
      in vm ++ [
        aliza # mri scans
        firefox
        dconf-editor
        gnome-terminal
        gimp
        gthumb
        gparted
        gnome-software
        kooha # screen recorder
        libreoffice-fresh
        # minecraft
        prismlauncher
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
        weasis
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
