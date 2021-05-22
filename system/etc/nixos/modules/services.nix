{ config, lib, pkgs, host, ... }: with builtins; with lib; with j; {
    services = {
        ${myIf.knull config.vars.zfs "sanoid"} = let
            sanoidBase = {
                useTemplate = [ "base" ];
                recursive = true;
            };
            disabled = { processChildrenOnly = true; };
        in {
            enable = true;
            templates."base" = {
                autoprune = true;
                autosnap = true;
        
                # 6 snapshots an hour
                daily = 144;
        
                # 2 snapshots a minute
                hourly = 120;
        
                # 6 snapshots a day for 28 days
                monthly = 168;
        
                # Twice the weeks in a year
                yearly = 104;
            };
        
            datasets = listToAttrs (map (dataset: nameValuePair "${host}/${dataset}" sanoidBase) [
                host
                "system/persist"
                "virt"
                "omniverse"
            ]);
        };

        ${myIf.knull config.vars.zfs "syncoid"} = let
            syncoidBase = mkMerge [{
                recursive = true;
                commonArgs = [
                    "--compress zstd-slow"
                    "--no-stream"
                    "--no-sync-snap"
                    "--create-bookmark"
                ];
                }
                (mkIf vars.encrypted {
                    sendOptions = "vvwRI";
                    recvOptions = "vvFs";
                })
                (mkIf (!vars.encrypted) {
                    recvOptions = "vvFds";
                    sendOptions = "vvRI";
                })
            ];
        in {
            enable = false;
            sshKey = "/root/.ssh/id_ecdsa";
            commands = listToAttrs (map (dataset: nameValuePair "${host}/${dataset}" (syncoidBase // { target = ""; })) [
                host
                "virt"
                "system/persist"
                "omniverse"
            ]);
        };

        wakeonlan.interfaces = map (interface:
            { inherit interface; method = "magicpacket"; }
        ) (attrNames config.networking.interfaces);

        flatpak.enable = !elem system [ "aarch64-linux" ];

        # For Yubikey
        udev.packages = with pkgs; [
            yubikey-personalization
            libu2f-host
        ];
        pcscd.enable = true;

        openssh = {
            enable = true;
            extraConfig = mkOrder 0 ''
            TCPKeepAlive yes
            ClientAliveCountMax 480
            ClientAliveInterval 3m
            '';
            permitRootLogin = "yes";
        };

        # Keybase
        kbfs.enable = true;
        keybase.enable = true;

        # Enable CUPS to print documents.
        printing.enable = true;

        # Enable the X11 windowing system.
        xserver = {
            enable = true;
            layout = "us";
            # xkbOptions = "eurosign:e";
            # Enable touchpad support.
            libinput = {
            enable = true;
            naturalScrolling = true;
            middleEmulation = true;
            tapping = true;
            };
            # synaptics.enable = true;
            desktopManager = { xterm.enable = false; };
            # "${ if (with config.vars; minimal || terminal) then null else "displayManager"}" = { defaultSession = "none+qtile"; };
            # "${ if (with config.vars; minimal || terminal) then null else "displayManager"}" = { startx.enable = true; };
            # "${ if (with config.vars; minimal || terminal) then null else "windowManager"}" = { qtile.enable = true; };
            # "${ if (with config.vars; minimal || terminal) then null else "displayManager"}" = { defaultSession = "none+exwm"; };
            # "${ if (with config.vars; minimal || terminal) then null else "windowManager"}" = { exwm.enable = true; };
            # displayManager = { defaultSession = "none+qtile"; };
            displayManager = { startx.enable = true; };
            windowManager = { qtile.enable = true; };
            # displayManager = { defaultSession = "none+exwm"; };
            # windowManager = { exwm.enable = true; };
            # desktopManager.gnome3.enable = true;
            # desktopManager.gnome3.enable = true;
            # displayManager.gdm.enable = true;
        };

        ${myIf.knull config.vars.zfs "zfs"} = {
            autoScrub.enable = true;

            # Managed by Sanoid
            autoSnapshot.enable = false;
        };
    };
}
