{ pkgs, ... }:

{
  config.home.packages = with pkgs; [
    amberol
    aliza
    meslo-lgs-nf
    bat
    bash
    bear
    haskellPackages.cabal-plan
    # haskellPackages.ghcup # broken package
    cachix
    vscode
    # gcc
    llvmPackages_16.libcxxClang
    gnome.dconf-editor
    gnome.gnome-terminal
    difftastic
    # dust
    # direnv
    # docker
    efibootmgr
    exa
    firefox
    fd
    fzf
    flatpak
    gnome.gnome-software
    # gnome-gedit
    # gimp
    glava
    gparted
    gnugrep
    gzip
    # gnome.pomodoro
    pomodoro
    gmp
    htop
    inxi
    jupyter
    just
    less
    lsof
    # man-pages
    micro
    monero-cli
    # npm
    # opam
    netcat-openbsd
    ncurses
    nix-melt
    # opendoas
    openssl
    procs
    refind
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
    unzip
    vim
    wget
    which
    xz
    yq
    # ytop
    jq
    zoxide
    zip
  ];
}
