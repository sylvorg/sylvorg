{ config, pkgs, ... }: with pkgs;

{
    environment = {
        pathsToLink = [ "/share/nix-direnv" ];
        systemPackages = [
            # bcachefs-tools
            acpilight
            alacritty
            asdf-vm
            assh
            atom
            autojump
            autossh
            bat
            bc
            btrfs-progs
            byobu
            cascadia-code
            cmake
            copyq
            coreutils
            ctop
            curl
            darling-dmg
            # ddar
            direnv
            diskus
            dos2unix
            duf
            elvish
            emacs
            entr
            exa
            exfat
            # extra-container
            fasd
            fd
            fd
            fff
            ffmpeg
            figlet
            filet
            firefox
            fish
            fzf
            gcc
            git
            git-crypt
            git-filter-repo
            git-fire
            git-lfs
            gitoxide
            glances
            gnumake
            google-chrome google-chrome-beta google-chrome-dev
            gotop
            gparted
            gptfdisk
            # haskellPackages.hocker
            inetutils
            # j-settings
            jupyter
            keybase-gui kitty
            libffi
            libguestfs
            libsForQt5.qtstyleplugin-kvantum
            libtool
            lolcat
            lorri
            man
            micro
            mkpasswd
            monkeysphere
            mosh
            mtr
            neo-cowsay
            neovim
            niv
            nix-direnv
            nnn
            nodePackages.prettier
            nox
            ntfs3g nixos-shell
            pandoc
            par2cmdline
            parted pmutils
            peru
            pfetch
            pypy
            python310
            python39Packages.pipx
            ranger
            refind
            ripgrep
            rsync
            sd
            shadowfox
            shellcheck
            silver-searcher
            snapper
            spacevim
            sqlite
            starship
            sysstat
            thefuck
            thermald
            tmux
            tmuxp
            tree
            udftools
            ulauncher
            uutils-coreutils
            vagrant
            vim
            vivaldi vivaldi-ffmpeg-codecs vivaldi-widevine
            vlc
            vscode vscodium
            wget
            win-qemu
            woeusb
            wtf
            xclip
            xclip
            xfce.thunar
            xz
            yubico-pam yubico-piv-tool yubikey-manager yubikey-agent yubikey-personalization yubioath-desktop
            yubikey-manager-qt yubikey-personalization-gui
            zenith
            zsh
        ] ++ (map (pkg: pkgs.gnome."gnome-${pkg}") [
            "boxes"
            "characters"
            "tweaks"
            "session"
        ]) ++ (map (pkg: pkgs."nix-prefetch-${pkg}") [
            "github"
            "docker"
            "scripts"
        ]) ++ (with pkgs.gnome; [
            dconf-editor
        ]) ++ (with pkgs.gitAndTools; [
            git-extras
            git-hub
            gitflow
            gh
            hub
            lab
        ]) ++ (with pkgs.python310Packages; [
            black
            black-macchiato
            poetry
            jupyterlab
            xonsh
        ]);
    };
}
