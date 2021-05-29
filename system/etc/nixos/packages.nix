inputs@{
    pkgs,
    lib,
    sources,
    stc,
    config,
    ...
} : with lib; with j; with stc; let
    inherit (config.vars) minimal terminal;
    inherit (attrs) versions;
    neither = !(minimal || terminal);
    mkifnt = myIf.drv (!terminal) pkgs.hello;
    mkifn = myIf.drv neither pkgs.hello;
    mkifnm = myIf.drv (!minimal) pkgs.hello;
    extra-container = let pkgSrc = sources.extraContainer; in pkgs.callPackage pkgSrc { inherit pkgSrc; };
in with pkgs; [
    autojump
    acpilight
    (myIf.drv (!elem system [ "aarch64-linux" ]) pkgs.hello appimage-run)
    (myIf.drv (!elem system [ "aarch64-linux" ]) pkgs.hello appimagekit)
    assh
    autossh
    bat
    bc
    bcachefs-tools
    btrfs-progs
    byobu
    cascadia-code
    copyq
    coreutils
    ctop
    curl
    darling-dmg
    ddar
    diskus
    dos2unix
    elvish
    entr
    exa
    exfat
    fasd
    fd
    fff
    ffmpeg
    ffmpeg
    figlet
    filet
    fzf
    gcc
    git
    git-crypt
    git-fire
    gotop
    gptfdisk
    inetutils
    libffi
    lolcat
    lorri
    micro
    mkpasswd
    monkeysphere
    mosh
    mtr
    neo-cowsay
    neovim
    niv
    nix-direnv
    nixops
    nnn
    nox
    ntfs3g
    pandoc
    par2cmdline
    parted
    peru
    pfetch
    pmutils
    ranger
    ripgrep
    rsync
    sd
    shellcheck
    silver-searcher
    snapper
    libguestfs
    qemu_xen_4_10-light
    starship
    sysstat
    thefuck
    thermald
    tmux
    tmuxp
    tree
    udftools
    uutils-coreutils
    vagrant
    vim
    wget
    win-qemu
    wtf
    xclip
    xenPackages.xen_4_10-light
    xz
    yadm
    yubico-pam
    yubico-piv-tool
    yubikey-manager
    yubikey-personalization
    yubioath-desktop
] ++ (map mkifn [
    gnome3.gnome-boxes
    gnome3.gnome-tweaks
    google-chrome
    google-chrome-beta
    google-chrome-dev
    vivaldi
    vivaldi-ffmpeg-codecs
    vivaldi-widevine
    vscodium
]) ++ (map mkifnm [
    extra-container
    haskellPackages.hocker
    refind
]) ++ (map mkifnt [
    alacritty
    atom
    (myIf.drv (!elem system [ "aarch64-linux" ]) pkgs.hello etcher)
    firefox
    gnome3.gnome-disk-utility
    gparted
    keybase-gui
    kitty
    libsForQt5.qtstyleplugin-kvantum
    shadowfox
    vlc
    vscode
    woeusb
    xclip
    xfce.thunar
    yubikey-manager-qt
    yubikey-personalization-gui
]) ++ (with pkgs.j.pkgset.20-09."emacs${versions.emacs}Packages"; [
    (mkifnm exwm)
]) ++ (with pkgs."python${versions.python}Packages"; [
    (myIf.drv (!elem system [ "aarch64-linux" ]) pkgs.hello pyls-black)
    pyls-black
    (mkifnm jupyter)
    nixpkgs
    poetry
]) ++ (with pkgs.gitAndTools; [
    git-annex
    git-extras
    git-hub
    gitflow
    gh
    hub
    lab
]) ++ (map (npp: mkifnm pkgs."nix-prefetch-${npp}") [
    "github"
    "docker"
    "scripts"
]) ++ (with pkgs.nur.repos; [
    (mkifnt onny.foliate)
]) ++ [
    # For emacs

    # org-roam
    sqlite

    # org-md-export-to-markdown
    nodePackages.prettier
] ++ (let
    p = "python${versions.python}Packages";
in [
    (getAttr "black" (
        if (pkgs ? black) then pkgs else pkgs."${p}"
    ))

    (myIf.drv (
        (pkgs ? black-macchiato) || (pkgs."${p}" ? black-macchiato)
    ) pkgs.hello (getAttr "black-macchiato" (
        if (pkgs ? black-macchiato) then pkgs else pkgs."${p}"
    )))

    (myIf.drv (pkgs ? duf) pkgs.hello (getAttr "duf" pkgs))
    (myIf.drv (pkgs ? gitoxide) pkgs.hello (getAttr "gitoxide" pkgs))
    (myIf.drv (pkgs ? glances) pkgs.hello (getAttr "glances" pkgs))
    (myIf.drv (pkgs ? nixos-shell) pkgs.hello (getAttr "nixos-shell" pkgs))
    (myIf.drv (pkgs ? obsidian && !neither) pkgs.hello (getAttr "obsidian" pkgs))
    (myIf.drv (pkgs ? yubikey-agent) pkgs.hello (getAttr "yubikey-agent" pkgs))
    (myIf.drv (pkgs ? zenith) pkgs.hello (getAttr "zenith" pkgs))
    (myIf.drv (pkgs."${p}" ? pipx) pkgs.hello (getAttr "pipx" pkgs."${p}"))
])
