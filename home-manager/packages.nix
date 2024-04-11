{ pkgs, ... }:

{
  config.home.packages = with pkgs; [
    amberol
    aliza
    alsa-utils
    alsa-scarlett-gui
    meslo-lgs-nf
    bat
    bash
    bear
    bitwig-studio
    bottom
    # nixpkgs-23-05.haskellPackages.cabal-plan
    # haskellPackages.ghcup # broken package
    cachix
    cpulimit
    cryptsetup
    ungoogled-chromium
    vscode
    gcc13
    gdb
    # clang_16
    # llvmPackages_16.libcxxClang
    gnome.dconf-editor
    gnome.gnome-terminal
    difftastic
    # dust
    # direnv
    # docker
    efibootmgr
    eza
    firefox
    fd
    fzf
    flatpak
    f2fs-tools
    gnome.gnome-software
    # gnome-gedit
    # gimp
    glava
    gparted
    gnugrep
    gzip
    helvum
    # gnome.pomodoro
    pomodoro
    gmp
    htop
    inxi
    jupyter
    just
    less
    lsof
    lshw
    usbutils
    # man-pages
    micro
    monero-cli
    # npm
    # opam
    netcat-openbsd
    ncurses
    nix-melt
    nix-tree
    nixos-generators
    # screen recorders:
    kooha
    obs-studio
    # opendoas
    openssl
    obsidian
    procs
    pv
    pciutils
    pinta
    # python3
    refind
    reaper
    rustup
    rm-improved
    # redux
    drumkv1
    hydrogen
    spotify
    sedutil
    gnused
    gnutar
    gimp
    gthumb
    # texinfo
    # texlive-bibtexextra
    texlive.combined.scheme-full
    tldr
    tor
    tor-browser
    # torsocks
    traceroute
    tree
    qemu
    gnome.gnome-boxes
    unzip
    vim
    wget
    which
    wineWowPackages.stableFull

    (lutris.override {
      extraLibraries = pkgs: with pkgs; [ openssl gnome.zenity ];
    })

    q4wine # qt-based gui for wine
    qjackctl
    qpwgraph
    carla
    jackmix
    jackmeter
    xz
    yq
    yabridge
    yabridgectl
    # ytop
    jq
    xclip
    zoxide
    zip
  ];
}
