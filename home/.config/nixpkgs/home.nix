with builtins; let
    base = { stc, all, flake, ... }:
        with builtins;
        with lib;
        with j;
        with stc;
        with integer-default-truths;
    let
        sources' = ((import (
            let
                lock = builtins.fromJSON (builtins.readFile /etc/nixos/flakes/home/flake.lock);
            in fetchTarball {
                url = "https://github.com/edolstra/flake-compat/archive/${lock.nodes.flake-compat.locked.rev}.tar.gz";
                sha256 = lock.nodes.flake-compat.locked.narHash; }
            ) {
                src = /etc/nixos/flakes/home;
            }).defaultNix).inputs;
        
        inherit (flake) all;
        
        fromAll = category: get {
            set = all.${category};
            stc = removeAttrs (
                attrNames attrs.home-manager-integer-defaults
            ) stc;
        };
        pkgs = fromAll "pkgs";
        overlays = fromAll "overlays";
        config = fromAll "config";
        
        integer-default-truths = mapAttrs (
            n: v: v == 1
        ) (filterAttrs (n: v: isInt v) stc);
        
        primary = {
            user = attrs.users.primary;
            home = attrs.allHomes.${primary.user};
        };
        homeDirectory = attrs.allHomes.${user};
        
        baseOptions = {
            # force = true;
        };
        
        userName  = primary.user;
        userEmail = "aiern@protonmail.com";
        
        links' = rec {
            cookiejar = {
                source = "/${host}/b/cj";
                target = "cj";
            };
            picotech = {
                source = "${cookiejar.source}/cc/.pico";
                target = ".pico";
            };
            byobu = {
                source = "${homeDirectory}/.tmux.conf";
                target = ".byobu/.tmux.conf";
            };
        };
        links = mapAttrs (link: st: {
            inherit (st) target;
            source = /. + st.source;
        }) (filterAttrs (link: st: pathExists (/. + st.source)) links');
        
        shellAliases = {
            n = "exit";
            c = "clear";
        };
        
        withLink = link: with link; { "${target}" = { inherit source; }; };
        
        hash = user: j.hostName { stc = stc // { inherit user; }; };
    in rec {
        inherit lib;
        nixpkgs = { inherit (stc) system; inherit overlays config; };
        fonts.fontconfig.enable = mkForce true;
        imports = [ "${sources.impermanence}/home-manager.nix" ];
        useUserPackages = true;
        useGlobalPkgs = true;
        backupFileExtension = "bak";
        verbose = true;

        # TODO
        sharedModules = [
            {  }
        ];

        extraSpecialArgs = flake.legacyPackages.make.specialArgs { inherit stc; };

        programs = foldToSet (map (set: mapAttrs (
            n: v: baseOptions // v
        ) set) (flatten [
            [
                {
                    home-manager = {
                        enable = true;
                        path = sources.home-manager.outPath;
                    };
                }
                ({
                    zsh = let zedFile = file: let
                        grml = "${sources.grml}/etc/zsh/${file}";
                    in myIf.empty (pathExists grml) (readFile grml); in {
                        enable = true;
                        enableAutosuggestions = true;
                        enableVteIntegration = true;
                        autocd = true;
                    
                        # TODO: Maybe this could be all paths in $PATH?
                        cdpath = [  ];
                    
                        defaultKeymap = "viins";
                        history.extended = true;
                        initExtra = '' '';
                        # initExtraFirst = '' '';
                        initExtraBeforeCompInit = (zedFile "zshrc") + ''
                            source ${homeDirectory}/.nix-profile/etc/profile.d/hm-session-vars.sh
                        '';
                        profileExtra = (zedFile "zprofile") + '' '';
                        envExtra = (zedFile "zshenv") + '' '';
                        loginExtra = (zedFile "zlogin") + '' '';
                        logoutExtra = (zedFile "zlogout") + '' '';
                        oh-my-zsh = {
                            enable = false;
                            plugins = [  ];
                            extraConfig = '' '';
                            # theme = "sushi";
                            custom = "";
                        };
                        plugins = [  ];
                        prezto = {
                            enable = true;
                            editor = {
                                dotExpansion = true;
                                keymap = "vi";
                            };
                            extraConfig = '' '';
                            extraFunctions = [  ];
                            extraModules = [  ];
                            pmodules = [  ];
                            pmoduleDirs = [  ];
                            # prompt.theme = "paradox";
                            tmux.itermIntegration = true;
                            utility.safeOps = true;
                        };
                        inherit shellAliases;
                        shellGlobalAliases = {  };
                    };
                    starship.enableZshIntegration = true;
                })
                ({
                    bash = {
                        enable = true;
                        enableVteIntegration = true;
                        initExtra = ''
                            source ${homeDirectory}/.nix-profile/etc/profile.d/hm-session-vars.sh
                        '';
                        inherit shellAliases;
                        shellOptions = [  ];
                    };
                    starship.enableBashIntegration = true;
                })
                ({
                    fish = {
                        enable = true;
                        package = pkgs.fish;
                        functions = shellAliases // {
                            sf = "source ~/.config/fish/config.fish";
                        };
                        interactiveShellInit = ''
                            bass source ${homeDirectory}/.nix-profile/etc/profile.d/hm-session-vars.sh
                        '';
                        plugins = [{ name = "bass"; src = sources'.bass; }];
                    };
                    starship.enableFishIntegration = true;
                })
                ({
                    git = {
                        # File at ~/.config/git/config
                        enable = true;
                        package = pkgs.git;
                        lfs.enable = true;
                        extraConfig = {
                            core = {
                                excludesfile = "${homeDirectory}/.gitignore";
                                attributesfile = "${homeDirectory}/.gitattributes";
                            };
                    
                            # TODO: Add the greater-than sizes
                            annex.largefiles = ''
                                include=**/__pycache__/* or \
                                include=**/dist/* or \
                                include=**/tests/* or \
                                include=**/deprecated/* or \
                                include=**/unfinished/* or \
                                include=**/.vscode/* or \
                                include=**/*.test.* or \
                                include=**/*.envrc
                            '';
                    
                        };
                        inherit userEmail userName;
                    };
                })
                ({
                    mercurial = {
                        enable = true;
                        package = pkgs.mercurial;
                        inherit userEmail userName;
                        ignoresRegexp = [];
                        extraConfig = {
                            extensions = {
                                # hgext = {
                                #     bookmarks = "";
                                #     convert = "";
                                # };
                                # hggit = "/usr/lib/python2.7/site-packages/hggit";
                                hggit = "";
                                strip = "";
                                share = "";
                            };
                            trusted = {
                                users = primary.user;
                                groups = primary.user;
                            };
                            git.blockdothg = "false";
                            ui.ignore = "${homeDirectory}/.hgignore";
                        };
                    };
                })
                ({
                    emacs = {
                        enable = true;
                        package = pkgs.emacsGcc;
                        overrides = final: prev: {
                            magit-delta = super.magit-delta.overrideAttrs (eprev: {
                                buildInputs = eprev.buildInputs ++ [ pkgs.git ];
                            });
                        };
                        # extraPackages = with pkgs."emacs${j.attrs.versions.emacs}Packages"; [
                        #     sqlite3
                        #     emacsql-sqlite3
                        # ];
                    };
                })
                ({
                    kitty = {
                        enable = true;
                        extraConfig = readFile "${homeDirectory}/${primary.user}/home/.config/kitty/kitty.conf";
                        settings.font_family = "Cartograph CF Light Italic";
                    };
                })
                ({
                    tmux = {
                        aggressiveResize = true;
                        customPaneNavigationAndResize = true;
                        enable = true;
                        extraConfig = let
                            # From: https://man7.org/linux/man-pages/man1/tmux.1.html#KEY_BINDINGS
                            unbindings = [ "C-b" "C-z" "C-x" ];
                            bindings = {  };
                            rootBindings = {  };
                            rootRepBindings = {
                                M-s = "send-prefix";
                                C-S-F5 = "send-keys M-F5";
                                C-S-Left = "send-keys M-Left";
                                C-S-Right = "send-keys M-Right";
                            };
                            repBindings = {  };
                            prefixTableBindings = {  };
                            plugins' = [
                                "tmux-plugins/tpm"
                                "jlipps/tmux-safekill"
                                "fcsonline/tmux-thumbs"
                                "addisonlynch/tmux-sidebar-plus"
                            ];
                            ccSS = command: list: concatStringsSep "\n" (map (_: "${command} ${_}") list);
                            ccSSS = command: set: concatStringsSep "\n" (
                                mapAttrsToList (n: v: "${command} ${n} ${v}") set
                            );
                            oh-my-tmux' = "source ${sources.oh-my-tmux}/.tmux.conf";
                            powerline' = "source ${sources.powerline}/powerline/bindings/tmux";
                        in concatStringsSep "\n" [
                    
                            (let
                              replacements = [
                                "unbind C-b"
                                "set -g prefix C-a"
                              ];
                            in replaceStrings replacements (map (r: "# " + r) replacements) (readFile "${sources.aleclearmind}/active-row.conf"))
                    
                            # (powerline' + "/powerline-base.conf")
                    
                            (powerline' + "/powerline.conf")
                    
                            # (powerline' + "/powerline_tmux_2.1_plus.conf")
                    
                            oh-my-tmux'
                    
                            "${oh-my-tmux'}.local"
                    
                            (ccSS "unbind" unbindings)
                    
                            (ccSSS "bind-key" bindings)
                    
                            (ccSSS "bind-key -n" rootBindings)
                    
                            (ccSSS "bind-key -nr" rootRepBindings)
                    
                            (ccSSS "bind-key -r" repBindings)
                    
                            (ccSSS "bind-key -T prefix" prefixTableBindings)
                    
                            ''
                                # set -g prefix2 S-Space
                                
                                # Mouse support - set to on if you want to use the mouse
                                # setw -g mode-mouse on
                                # set -g mouse-select-pane on
                                # set -g mouse-resize-pane on
                                # set -g mouse-select-window on
                                
                                # enable activity alerts
                                setw -g monitor-activity off
                                set -g visual-activity off
                                
                                # Center the window list
                                set -g status-justify centre
                                
                                # VI Mode
                                set -g status-keys vi
                                
                                # utf8 is on
                                # set -g utf8 on
                                # set -g status-utf8 on
                                
                                run-shell "powerline-daemon -q"
                                
                                set -g status-right '#{prefix_highlight} | %a %Y-%m-%d %H:%M'
                                
                                # Spacemacs Settings:
                                set -gs escape-time 10
                                
                                # address vim mode switching delay (http://superuser.com/a/252717/65504)
                                set -s escape-time 0
                                
                                # increase scrollback buffer size
                                set -g history-limit 50000
                                
                                # tmux messages are displayed for 1.25 seconds
                                set -g display-time 1250
                                
                                # refresh 'status-left' and 'status-right' more often
                                # set -g status-interval 1
                                
                                # focus events enabled for terminals that support them
                                set -g focus-events on
                                
                                # border thickness
                                set-option -g pane-active-border-style "bg=default"
                                set-option -ag pane-active-border-style "fg=colour208"
                                set -g status-right '#{prefix_highlight} | %a %Y-%m-%d %H:%M'
                                
                                # Adapted From: https://www.reddit.com/r/tmux/comments/einuqy/make_tmux_modal/
                                set-option -g prefix None
                                bind-key -n C-Space {
                                  set-option key-table prefix
                                  set-option status-bg yellow
                                }
                                bind-key -T prefix C-Space {
                                  set-option key-table root
                                  set-option status-bg green
                                }
                            ''
                    
                            (ccSS "set -g @plugin" plugins')
                    
                            ''run "${sources.tpm}/tpm"''
                        ];
                        keyMode = "vi";
                        newSession = true;
                        package = pkgs.tmux;
                        secureSocket = false;
                        shortcut = "Space";
                        terminal = "tmux-256color";
                        tmuxp.enable = false;
                        # tmuxifier.enable = false;
                        # plugins = with pkgs; with tmuxPlugins; [
                        #     battery
                        #     cpu
                        #     ctrlw
                        #     fpp
                        #     fzf-tmux-url
                        #     (myIf.drv (tmuxPlugins ? jump) tmuxPlugins.battery (getAttr "jump" tmuxPlugins))
                        #     logging
                        #     online-status
                        #     open
                        #     pain-control
                        #     prefix-highlight
                        #     sessionist
                        #     sidebar
                        #     sysstat
                        #     (myIf.drv (tmuxPlugins ? tmux-fzf) tmuxPlugins.battery (getAttr "tmux-fzf" tmuxPlugins))
                        #     (myIf.drv (tmuxPlugins ? vim-tmux-focus-events) tmuxPlugins.battery (getAttr "vim-tmux-focus-events" tmuxPlugins))
                        #     vim-tmux-navigator
                        #     yank
                        #     (let
                        #         r = "resurrect";
                        #         processes = [
                        #             '' "mc --nocolor" ''
                        #             '' "tail -f" ''
                        #             "bat"
                        #             "docker"
                        #             "elvish"
                        #             "emacs"
                        #             "fish"
                        #             "glances"
                        #             "gotop"
                        #             "htop"
                        #             "ipython"
                        #             "irssi"
                        #             "less"
                        #             "man"
                        #             "more"
                        #             "mosh"
                        #             "mutt"
                        #             "nvim"
                        #             "ssh"
                        #             "syncthing"
                        #             "tail"
                        #             "top"
                        #             "vi"
                        #             "vim"
                        #             "weechat"
                        #             "wtf"
                        #             "xonsh"
                        #             "xsh"
                        #             "zsh"
                        #         ];
                        #     in {
                        #         plugin = tmuxPlugins.${r};
                        #         extraConfig = ''
                        #             set -g @${r}-save-bash-history 'on'
                        #             set -g @${r}-dir '~/.byobu/tmux_${r}'
                        #             set -g @${r}-processes '${concatStringsSep " " processes}'
                        #             set -g @${r}-capture-pane-contents 'on'
                        #             set -g @${r}-strategy-vim 'session'
                        #             set -g @${r}-strategy-nvim 'session'
                        #         '';
                        #     })
                        #     (let c = "continuum"; in {
                        #         plugin = tmuxPlugins.${c};
                        #         extraConfig = ''
                        #             set -g @${c}-boot 'on'
                        #             set -g @${c}-save-interval '10'
                        #             set -g @${c}-restore 'on'
                        #         '';
                        #     })
                        # ];
                    };
                })
                ({
                    starship = {
                        enable = true;
                        package = pkgs.starship;
                        settings = {
                    
                        };
                    };
                })
                ({
                    direnv = {
                        # File at ~/.config/direnv/direnvrc
                        enable = true;
                        enableBashIntegration = true;
                        enableFishIntegration = true;
                        enableNixDirenvIntegration = true;
                        enableZshIntegration = true;
                        stdlib = ''
                            use_flake() {
                                watch_file flake.nix
                                watch_file flake.lock
                                eval "$(nix print-dev-env --profile "$(direnv_layout_dir)/flake-profile")"
                            }
                        '';
                    };
                })
            ]
        ]));

        services = {
            gpg-agent = {
                enable = true;
                enableSshSupport = false;
                pinentryFlavor = "curses";
            };
            emacs = {
                enable = true;
                package = if (programs ? emacs) then programs.emacs.package else pkgs.emacsGcc;
            };
        };

        home = let
            persistence = {
                ${
                    myIf.knull (nixos && zfs) "persistence"
                } = attrs.persistence.home { inherit user; };
            };

            seqList = [

                # !!! CAREFUL! THE ORDER HERE MATTERS! !!!
                persistence

            ];

        in sequence seqList (foldToSet [{
                packages = import ../../etc/nixos/packages.nix { inherit stc lib pkgs; };
                file = foldToSet (map (set: mapAttrs (
                    n: v: { force = true; } // v
                ) set) (flatten [
                    [
                        ({ ".hgignore".source = "${homeDirectory}/${primary.user}/home/.hgignore"; })
                        (let source = "/persist/home/root/${primary.user}"; in foldToSet [
                            (myIf.set (user == "root") (attrs.link "/" "${source}/system"))
                            (attrs.link homeDirectory "${source}/home")
                            {
                                "${primary.user}".source = source;
                                ${myIf.knull (user == "root") "/usr/local/etc/doas.conf"}.text = ''
                                    permit keepenv :wheel
                                    permit nopass keepenv root as root
                                '';
                            })
                            ({
                                ".gitignore".text = "${homeDirectory}/${primary.user}/home/.gitignore";
                                ".gitattributes".text = "${homeDirectory}/${primary.user}/home/.gitattributes";
                            })
                            ({
                                ".xonshrc".text = let
                                    python_ver = concatStringsSep "." (stringToCharacters attrs.versions.python);
                                    Channel = toCapital channel;
                                in with attrs.commands; ''
                                homeManager = True
                                
                                # Note that type hints cannot be used
                                
                                tmux attach 2> /dev/null
                                # pfetch
                                
                                try:
                                    if homeManager:
                                        python_ver = "${python_ver}"
                                        home = fp("${homeDirectory}")
                                        xeroFigletFonts = "${sources.xeroFigletFonts}"
                                except NameError:
                                    python_ver = "3.9"
                                    home = fp("~")
                                    xeroFigletFonts = fp("~/shadowrylander/resources/xeroFigletFonts")
                                
                                # figlet -d @(xeroFigletFonts) -f smisome1.flf "Hello!" | lolcat
                                from sys import path as sys_path
                                from datetime import datetime
                                from nanite import fullpath as fp
                                from os import path as os_path, sep as os_sep
                                from inspect import getsourcefile
                                source-bash ~/.nix-profile/etc/profile.d/hm-session-vars.sh
                                $AUTO_CD = True
                                $AUTO_PUSHD = True
                                $AUTO_SUGGEST = True
                                $AUTO_SUGGEST_IN_COMPLETIONS = True
                                $BASH_COMPLETIONS = [
                                    "${sources.bashCompletions}",
                                    "./resources/bashCompletions",
                                ]
                                $COMPLETION_IN_THREAD = True
                                $COMPLETIONS_CONFIRM = True
                                $DOTGLOB = True
                                $FUZZY_PATH_COMPLETION = True
                                $MOUSE_SUPPORT = True
                                $PRETTY_PRINT_RESULTS = True
                                $PROMPT_TOOLKIT_COLOR_DEPTH = "DEPTH_24_BIT"
                                $SHELL_TYPE = "prompt_toolkit"
                                $UPDATE_COMPLETIONS_ON_KEYPRESS = True
                                $UPDATE_OS_ENVIRON = True
                                $VI_MODE = True
                                $XONSH_AUTOPAIR = True
                                $XONSH_CACHE_EVERYTHING = True
                                
                                # This enumerates all history files when set to true
                                $XONSH_DEBUG = False
                                
                                $XONSH_SHOW_TRACEBACK = True
                                
                                # Xonsh Prompt
                                $PROMPT_FIELDS["prompt_end"] = "Wheee! 😹 "
                                # $PROMPT = "{BOLD_#E5004D}{env_name} {BOLD_#FF4081}{prompt_end}"
                                $PROMPT = lambda: $(starship prompt)
                                $RIGHT_PROMPT = "{BOLD_#E5004D} {prompt_end}{BOLD_#FC9F71} || {BOLD_#E5004D} {user}@{hostname} "
                                # $BOTTOM_TOOLBAR = $RIGHT_PROMPT
                                
                                # Path
                                sys_path.insert(0, "")
                                extra_paths = [
                                    f"{home}/.nimble/bin",
                                    f"{home}/go/bin",
                                    "/usr/lib/node_modules",
                                ]
                                $PATH += extra_paths
                                if os_path.exists(fp("~/git-annex.linux")):
                                   $PATH += fp("~/git-annex.linux")
                                
                                # Shell
                                # $EDITOR = "emacsclient --socket-name=spacemacsd -t"
                                $EDITOR = "vim"
                                $TERM = "xterm-kitty"
                                $GPG_TTY=$(tty)
                                
                                # Etc
                                # $DIRENV_WARN_TIMEOUT = "100y"
                                $LESSOPEN = "| /usr/share/source-highlight/src-hilite-lesspipe.sh %s"
                                $LESS = " -R "
                                
                                # Linuxbrew
                                # $LINUXBREWHOME = "/home/linuxbrew/.linuxbrew"
                                # $PATH.append("$LINUXBREWHOME/bin")
                                # $MANPATH = ["$LINUXBREWHOME/man"]
                                # $PKG_CONFIG_PATH = ["$LINUXBREWHOME/lib64/pkgconfig", "$LINUXBREWHOME/lib/pkgconfig"]
                                # $LD_LIBRARY_PATH = ["$LINUXBREWHOME/lib64", "$LINUXBREWHOME/lib"]
                                def _yadm(args, stdin=None):
                                    if args[0] == "edit":
                                        vim @(fp(f"{args[1]}##template"))
                                    else:
                                        # From https://github.com/xonsh/xonsh/issues/3159#issuecomment-500364622
                                        $(which -s yadm) @(args)
                                
                                def _mdg(args, stdin=None):
                                    mkdir -p @(args)
                                    cd @(args[0])
                                
                                # quick opening files with vim
                                def _fasdv(args, stdin=None):
                                    fasd -fe vim @(args)
                                
                                def hgsetup(args, stdin=None):
                                    hg bookmark master
                                    hg checkout master
                                    hg add .hgignore
                                    hg ci -m .hgignore
                                    hg ci -Am @(args)
                                    hg push
                                
                                def _alacritty_change_themes(args, stdin=None):
                                    _alc = "alacritty"
                                    _alc_theme = fullpath(
                                        config_dir,
                                        _alc,
                                        "themes",
                                        args[0]+".yaml"
                                    )
                                    _alc_config = fullpath(
                                        "~",
                                        ".config",
                                        _alc,
                                        _alc+".yml"
                                    )
                                    rsync @(_alc_theme) @(_alc_config)
                                
                                def _compile(args, stdin=None):
                                    clear
                                    name = args.pop(0)
                                    g++ @(name + ".cpp")
                                    ./a.out @(name) @(args)
                                
                                def _compile_only(args, stdin=None):
                                    g++ @(args[0] + ".cpp")
                                
                                def _compile_header(args, stdin=None):
                                    g++ @(args[0] + ".h")
                                
                                def _compile_link(args, stdin=None):
                                    for arg in args:
                                        g++ @(arg + ".cpp") -c
                                    args = [arg + ".o" for arg in args]
                                    g++ @(args) -o a.out
                                
                                def _direnv_allow(args, stdin=None):
                                    if args:
                                        for argument in args:
                                            direnv allow @(argument)
                                    else:
                                        direnv allow
                                
                                def _la(args, stdin=None):
                                    if "-t" in args:
                                        args = list(args)
                                        args.remove("-t")
                                        exa -laT @(args)
                                    else:
                                        exa -la @(args)
                                
                                def _evim(args, stdin=None):
                                    emacsclient --socket-name=doom -t @(args)
                                
                                def _git(args, stdin=None):
                                    $(which -s hub) @(args)
                                
                                def _ssh(args, stdin=None):
                                    $(which -s assh) wrapper ssh @(args)
                                # xontrib load autojump
                                # xontrib load direnv
                                # xontrib load docker_tabcomplete
                                # xontrib load prompt_bar
                                # xontrib load prompt_ret_code
                                # xontrib load ssh_agent
                                # xontrib load vox_tabcomplete
                                xontrib load abbrevs
                                xontrib load autoxsh
                                xontrib load bashisms
                                xontrib load coreutils
                                xontrib load fzf-widgets
                                xontrib load kitty
                                xontrib load pipeliner
                                xontrib load readable-traceback
                                xontrib load schedule
                                xontrib load vox
                                xontrib load whole_word_jumping
                                xontrib load z
                                
                                # Jedi was what was causing the python function completions
                                # instead of the path completions
                                # xontrib load jedi
                                # byobu attach -t shadowrylander &> /dev/null
                                
                                from nanite import fullpath as fp
                                from os import getcwd
                                
                                if getcwd() == "/":
                                    cd ~
                                
                                xtras = fp(home, ".xonsh.d")
                                fzf_dir_file = fp(xtras, "fzf_tmux_dir_file.sh")
                                
                                source @(fp(xtras, "passwords.xsh"))
                                source @(fp(xtras, "environ.xsh"))
                                source @(fp(xtras, "functions.xsh"))
                                source @(fp(xtras, "xontribs.xsh"))
                                # source-zsh $(fasd --init auto)
                                # source-zsh @(fp(home, ".nix-profile", "etc", "profile.d", "hm-session-vars.sh"))
                                
                                # vox create base -p $(which @(f"python{python_ver}")) 2> /dev/null
                                # vox enter base 2> /dev/null
                                
                                # aliases["act"] = _alacritty_change_themes
                                aliases["evim"] = _evim
                                # aliases["hgsetup"] = hgsetup
                                aliases["-"] = "pushd"
                                aliases[".."] = "cd .."
                                aliases["."] = "la ."
                                aliases["ba"] = "byobu attach"
                                aliases["bd"] = "byobu detach"
                                aliases["bda"] = "byobu detach-client -a"
                                aliases["c"] = "clear"
                                aliases["compile-header"] = _compile_header
                                aliases["compile-link"] = _compile_link
                                aliases["compile-only"] = _compile_only
                                aliases["compile"] = _compile
                                aliases["da"] = _direnv_allow
                                aliases["emd"] = "emacs --bg-daemon=doom"
                                aliases["kemd"] = "emacsclient --socket-name=doom -e '(kill-emacs)'"
                                aliases["la"] = _la
                                aliases["md"] = lambda args, stdin=None: $(mkdir -p @(args))
                                aliases["mdg"] = _mdg
                                aliases["mosh"] = lambda args, stdin=None: $($(which -s mosh) --experimental-remote-ip=remote @(args))
                                aliases["n"] = "exit"
                                aliases["remd"] = "emacsclient --socket-name=doom -e '(kill-emacs)' && emacs --bg-daemon=doom"
                                # aliases["ssh"] = _ssh
                                aliases["ve"] = lambda args, stdin=None: $(vox enter @(args)) if args else $(vox exit)
                                aliases["yadm"] = _yadm
                                aliases["git"] = _git
                                
                                # fasd aliases
                                aliases["o"] = lambda args, stdin=None: $(fasd -ae xdg-open @(args)) # quick opening files with xdg-open
                                aliases["v"] = _fasdv # quick opening files with vim
                                
                                _fzf_df_alias = "..."
                                def _fzf(args, stdin=True):
                                    args = list(args)
                                    dir_file = $(sh @(fzf_dir_file) @(fp(args.pop(0)))).rstrip()
                                    if args[0] == _fzf_df_alias:
                                        _fzf([dir_file] + args[1::])
                                    else:
                                        @(args) @(dir_file)
                                aliases[_fzf_df_alias] = _fzf
                                '' + (myIf.empty nixos ''
                                
                                #################
                                # NixOS ${Channel}
                                #################
                                
                                aliases["rebuild"] = lambda args, stdin=None: $(${rebuild} @(args) -p ${host})
                                aliases["install"] = lambda args, stdin=None: $(${install})
                                
                                # TODO: Revise this
                                def _deploy(args, stdin=None):
                                    sudo nixops create /etc/nixos/nixops/home.nix -d home
                                    sudo nixops set-args --arg hostName ${get {
                                        set = all.hostName;
                                        stc = (filterAttrs (n: v: elem n j.attrs.stc) stc) // { inherit (stc) host; };
                                    }} -d home
                                    if "all" in args:
                                        sudo nixops deploy -d home
                                aliases["deploy"] = _deploy
                                '');
                            })
                            ({
                                "rc.elv".source = "${homeDirectory}/${primary.user}/home/rc.elv";
                            })
                            ({
                                ".config/ion/initrc".source = "${homeDirectory}/${primary.user}/home/.config/ion/initrc";
                            })
                            ({
                                ".config/powershell/Microsoft.PowerShell_profile.ps1".source = "${homeDirectory}/${primary.user}/home/.config/powershell/Microsoft.PowerShell_profile.ps1";
                            })
                            ({
                                ".config/nvim" = {
                                    source = sources.spacevim;
                                    recursive = true;
                                };
                                ".vim" = {
                                    source = sources.spacevim;
                                    recursive = true;
                                };
                                ".SpaceVim.d/init.toml".source = "${homeDirectory}/${primary.user}/home/.SpaceVim.d/init.toml";
                                ".SpaceVim.d/autoload/vimrc.vim".source = "${homeDirectory}/${primary.user}/home/.SpaceVim.d/autoload/vimrc.vim";
                            })
                            ({
                                # ".doom.d/system_init.el".text = ''
                                #   (add-to-list 'exec-path "${pkgs."emacs${j.attrs.versions.emacs}Packages".emacsql-sqlite3}/bin")
                                #   (add-to-list 'exec-path "${pkgs."emacs${j.attrs.versions.emacs}Packages".sqlite3}/bin")
                                # '';
                                ".doom.d".source = "${homeDirectory}/${primary.user}/home/.doom.d";
                                ".emacs.d".source = sources.doom-emacs;
                            })
                            ({
                                ".tmuxp/default.yaml".text = "session_name: default";
                            })
                            ({
                                ".direnvrc".source = "${homeDirectory}/${primary.user}/home/.direnvrc";
                            })
                            ({
                                ".config/qtile/config.py".source = "${homeDirectory}/${primary.user}/home/.config/qtile/config.py";
                            })
                            ({
                                kitty = {
                                    enable = true;
                                    extraConfig = readFile "${homeDirectory}/${primary.user}/home/.config/kitty/kitty.conf";
                                    settings.font_family = "Cartograph CF Light Italic";
                                };
                            })
                            ({
                                ".config/nixpkgs/overlays.nix".source = "${homeDirectory}/${primary.user}/home/.config/nixpkgs/overlays.nix";
                                ".config/nixpkgs/config.nix".source = "${homeDirectory}/${primary.user}/home/.config/nixpkgs/config.nix";
                                ".config/nix/nix.conf".text = attrs.configs.nix;
                            })
                        ]
                    ]
                    (map withLink (attrValues links))
                ]));
            }
            persistence
        ]);
    };

    flake = (import (
        let
            lock = builtins.fromJSON (builtins.readFile /etc/nixos/flake.lock);
        in fetchTarball {
            url = "https://github.com/edolstra/flake-compat/archive/${lock.nodes.flake-compat.locked.rev}.tar.gz";
            sha256 = lock.nodes.flake-compat.locked.narHash; }
        ) {
            src =  /etc/nixos;
        }).defaultNix
    inherit (flake) all;

in with lib; with j; forAllSystems' {
    inherit all;
    func = base;
    inherit extraListSets;
    inheritance = { inherit all flake; };
}
