{ pkgs, ... }:

{
  config.home.packages = with pkgs; [
    meslo-lgs-nf
    bash
    bear
    haskellPackages.cabal-plan
    # haskellPackages.ghcup # broken package
    vscode
    # gcc
    llvmPackages_16.libcxxClang
    gnome.dconf-editor
    gnome.gnome-terminal
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
    just
    less
    lsof
    # man-pages
    micro
    monero-cli
    # npm
    # opam
    netcat-openbsd
    # opendoas
    openssl
    refind
    rustup
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
}
