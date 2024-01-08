{ pkgs, ... }:

{
  config.home.packages = with pkgs; [
    amberol
    aliza
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
    # opendoas
    openssl
    obsidian
    procs
    pv
    pciutils
    # python3
    refind
    reaper
    rustup
    rm-improved
    spotify
    gnused
    gnutar
    # texinfo
    # texlive-bibtexextra
    texlive.combined.scheme-full
    tldr
    tor
    # torsocks
    traceroute
    tree
    qemu
    gnome.gnome-boxes
    unzip
    vim
    wget
    which
    xz
    yq
    # ytop
    jq
    xclip
    zoxide
    zip
  ];
}
