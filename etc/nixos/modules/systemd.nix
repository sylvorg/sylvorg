{ config, lib, pkgs, host, ... }: with lib;

{
    systemd = let
        replaceBorgmatic = n: replaceStrings [ "borgmatic-" ] [ "" ] n;
        replaceRclone = n: replaceStrings [ "rclone-" ] [ "" ] n;
        mkBaseWants = list: unique ([ "network.target" "network-online.target" ] ++ list);
    in {
        # packages = with pkgs; [ runit ];
        services = let
            mkBaseBorgmaticWants = list: mkBaseWants ([ "rclone-backblazeB2.service" ] ++ list);
            mkBaseBorgmatic = n: rec {
                description = "Borgmatic ${(j.toCapital n)} Backup";
                unitConfig.ConditionACPower = "true";
                serviceConfig = {
                    Type = "oneshot";
                    MemoryDenyWriteExecute = "no";
                    LockPersonality = "true";
                    MemoryDenyWriteExecute=no
                    NoNewPrivileges = "yes";
                    PrivateDevices = "yes";
                    PrivateTmp = "yes";
                    ProtectClock = "yes";
                    ProtectControlGroups = "yes";
                    ProtectHostname = "yes";
                    ProtectKernelLogs = "yes";
                    ProtectKernelModules = "yes";
                    ProtectKernelTunables = "yes";
                    RestrictAddressFamilies = "AF_UNIX AF_INET AF_INET6 AF_NETLINK";
                    RestrictNamespaces = "yes";
                    RestrictRealtime = "yes";
                    RestrictSUIDSGID = "yes";
                    SystemCallArchitectures = "native";
                    SystemCallFilter = "@system-service";
                    SystemCallErrorNumber = "EPERM";
                    ProtectSystem = "strict";
                    ProtectHome = "tmpfs";
                    Nice = "19";
                    CPUSchedulingPolicy = "batch";
                    IOSchedulingClass = "best-effort";
                    IOSchedulingPriority = "7";
                    IOWeight = "100";
                    Restart = "no";
                    LogRateLimitIntervalSec = "0";
                    ExecStartPre = "${pkgs.coreutils}/bin/sleep 10m";
                    ExecStart = ''
                        ${pkgs.systemd}/bin/systemd-inhibit --who=\"${description}\" \
                                                            --why=\"Prevent interrupting scheduled backup for `${description}'\" \
                                                            ${pkgs.borgmatic}/bin/borgmatic \
                                                                --verbosity -1 \
                                                                --syslog-verbosity 1 \
                                                                --config /etc/borgmatic/${n}.yaml
                    '';
                };
            };
            mkBaseRclone = n: let
                mountdir = "/mnts/rclone/${n}";
            in rec {
                description = "Rclone ${j.toCapital n} Mount";
                wants = mkBaseWants [  ];
                after = wants;
                serviceConfig = {
                    ExecStartPre = "/run/current-system/sw/bin/mkdir -p ${mountdir}";
                    ExecStop = "/run/wrappers/bin/fusermount -u ${mountdir}";
                    RestartSec = "10s";
                    Type = "notify";
                };
            };
            mkBaseExecStartRclone = mount: let
                dir-cache-time = "96h";
                buffer-size = "512M";
            in ''
                ${pkgs.rclone}/bin/rclone mount \
                                          ${mount} \
                                          /mnts/rclone/${head (splitString ":" mount)} \
                                          --config ${j.attrs.homes.primary}/rclone.conf \
                                          --cache-dir /var/rclone \
                                          --dir-cache-time ${dir-cache-time} \
                                          --vfs-cache-mode full \
                                          --vfs-cache-max-age ${dir-cache-time} \
                                          --vfs-read-chunk-size 128M \
                                          --vfs-read-chunk-size-limit ${buffer-size} \
                                          --buffer-size ${buffer-size} \
                                          --umask 022 \
                                          --allow-other \
                                          --allow-root
            '';
        in mkMerge [
            (mapAttrs (n: v: recursiveUpdate (recursiveUpdate j.attrs.configs.services.base (mkBaseBorgmatic (replaceBorgmatic n))) v) {
                borgmatic-oreo = rec {
                    wants = mkBaseBorgmaticWants [ "chimchar-oreo.mount" "oreo.mount" ];
                    after = wants;
                    serviceConfig = {
                        ReadWritePaths = "-/oreo";
                        ReadOnlyPaths = "-/chimchar/oreo";
                    };
                };
                borgmatic-oreo-rsync = rec {
                    wants = mkBaseBorgmaticWants [ "chimchar-oreo.mount" ];
                    after = wants;
                    serviceConfig.ReadOnlyPaths = "-/chimchar/oreo";
                };
                borgmatic-chimchar = rec {
                    wants = mkBaseBorgmaticWants [ "chimchar.mount" "infernape.mount" ];
                    after = wants;
                    serviceConfig = {
                        ReadWritePaths = "-/infernape";
                        ReadOnlyPaths = "-/chimchar";
                    };
                };
                borgmatic-user = rec {
                    wants = mkBaseBorgmaticWants [ "${replaceStrings [ "/" ] [ "-" ] (removeSuffix "/" (removePrefix "/" j.attrs.homes.primary))}.mount" ];
                    after = wants;
                    serviceConfig = {
                        BindPaths = "-/home/shadowrylander/aiern -${j.attrs.homes.primary}/.user";
                    };
                };
            })
            (mapAttrs (n: v: recursiveUpdate (recursiveUpdate j.attrs.configs.services.base (mkBaseRclone (replaceRclone n))) v) {
                rclone-backblazeB2.serviceConfig.ExecStart = mkBaseExecStartRclone "backblazeB2:borgbackups-53f2bd74-148c-4fe7-bdb5-701d325645a6";
            })
            {
                # wstunnel-http = mkIf config.variables.relay (recursiveUpdate (j.attrs.configs.services.mkBase "root") {
                #     serviceConfig = {
                #         ExecStart = "${pkgs.wstunnel}/bin/wstunnel --server ws://0.0.0.0:80 -r 127.0.0.1:880";
                #     };
                # });
                # iodine = mkIf config.variables.relay (recursiveUpdate (j.attrs.configs.services.mkBase "root") {

                # });
                # wstunnel-tls = mkIf config.variables.relay (recursiveUpdate (j.attrs.configs.services.mkBase "root") {
                wstunnel = mkIf config.variables.relay (recursiveUpdate (j.attrs.configs.services.mkBase "root") {
                    serviceConfig = {
                        ExecStart = "${pkgs.wstunnel}/bin/wstunnel wss://0.0.0.0:443 -r 127.0.0.1:32443";
                    };
                });
            }
        ];
        timers = let
            mkBase = n: {
                description = "Borgmatic ${toCapital n} Backup Timer";
                wantedBy = [ "timers.target" ];
                timeConfig = {
                    OnCalender = "*-*-* */6:00:00";
                    Persistent = "true";
                    RandomizedDelaySec = "3h";
                };
            };
        in mapAttrs (n: v: recursiveUpdate (mkBase (replaceBorgmatic n)) v) {
            borgmatic-oreo = {  };
            borgmatic-oreo-rsync = {  };
            borgmatic-chimchar = {  };
            borgmatic-user = {
                timeConfig = {
                    OnCalender = "*-*-* */3:00:00";
                    RandomizedDelaySec = "90min";
                };
            };
        };
    };
}
