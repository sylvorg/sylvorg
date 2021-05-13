{ config, lib, pkgs, ... }: with builtins; with lib; with j; {
    boot = let
        p = attrs.users.primary;
        pHome = config.users.users.${p}.home;
    in mkMerge [{
            loader = {
                systemd-boot = {
                    enable = mkForce config.vars.bootPart;
                    configurationLimit = 25;
                    editor = false;
                };
                grub = {
                    enable = mkForce false;
                    efiSupport = true;
                    devices = [ "nodev" ];
                    version = 2;
            
                    # TODO: Get more options
                    extraEntries = ''
                        menuentry "Reboot" { reboot }
                        menuentry "Poweroff" { halt }
                    '';
            
                };
                efi = {
                    canTouchEfiVariables = mkForce true;
                    efiSysMountPoint = "/boot/efi";
                };
                timeout = 10;
            
                # Used for Bedrock Linux
                initScript.enable = mkFroce true;
            };
            supportedFilesystems = attrs.fileSystems.supported;
            initrd = {
                inherit (config.boot) supportedFilesystems;
                compressor = "${lib.getBin pkgs.zstd}/bin/zstd";
            };
            postBootCommands = let
                chowned = concatStringsSep "\n" (
                    map (user:
                        "sudo -u ${user} chown -R ${user}:${user} /persist/${attrs.allHomes.${user}} /persist/cache/${user}")
                    attrs.allUsers);
            in mkAfter (''
                mkdir -p /mnt
                sudo -u ${p} chown -R ${p}:${p} /${host}
            '' + chowned);
        }
        (mkIf (!config.vars.minimal) {
            extraModprobeConfig = '' options kvm_intel_nested=1 '';
        })
    ];
}
