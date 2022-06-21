{ config, pkgs, ... }: with pkgs;

{
    environment = {
        pathsToLink = [ "/share/nix-direnv" ];
        systemPackages = [
            # ddar
            # extra-container
            # haskellPackages.hocker
            acpilight
            alacritty
            asdf-vm
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
            direnv
            diskus
            distrobox
            dos2unix
            duf
            elvish
            emacs
            entr
            exa
            exfat
            fasd
            fd
            fd
            fff
            ffmpeg
            figlet
            filet
            firefox
            fzf
            gcc
            git
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
            inetutils
            jupyter
            keybase-gui kitty
            libffi
            libguestfs
            libsForQt5.qtstyleplugin-kvantum
            libtool
            lolcat
            lorri
            man
            meld
            micro
            mkpasswd
            monkeysphere
            mtr
            neo-cowsay
            neovim
            nickel
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
            Python
            ranger
            refind
            ripgrep
            rsync
            sd
            settings
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
            wstunnel
            wtf
            xclip
            xclip
            xfce.thunar
            xz
            zenith
            zoxide
        ] ++ (map (pkg: pkgs.gnome."gnome-${pkg}") [
            "boxes"
            "characters"
            "session"
            "tweaks"
        ]) ++ (map (pkg: pkgs."nix-prefetch-${pkg}") [
            "docker"
            "github"
            "scripts"
        ]) ++ (with pkgs.gnome; [
            dconf-editor
        ]) ++ (with pkgs.gitAndTools; [
            gh
            git-extras
            git-hub
            gitflow
            hub
            lab
        ]) ++ (with pkgs.PythonPackages; [
            black
            black-macchiato
            jupyterlab
            poetry
            pipx
        ]);
    };
}
