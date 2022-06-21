{ config, options, lib, ... }: with lib;

{
    options = {
        variables = {
            zfs = mkOption {
                type = types.bool;
                default = true;
            };
            relay = mkOption {
                type = types.bool;
                default = false;
            };
            server = mkOption {
                type = types.bool;
                default = config.variables.relay;
            };
            client = mkOption {
                type = types.bool;
                default = (! config.variables.server) && (! config.variables.relay);
            };
            minimal = mkOption {
                type = types.bool;
                default = false;
            };
            encrypted = mkOption {
                type = types.bool;
                default = false;
            };
        };
        programs = {
            mosh = {
                openFirewall = mkOption {
                    type = types.bool;
                    default = false;
                    description = "Whether to automatically open the specified port in the firewall.";
                };
            };
        };
        services = {
            tailscale = {
                autoconnect = mkOption {
                    type = types.bool;
                    default = false;
                    description = "Automatically run `tailscale up' on boot.";
                };
                openFirewall = mkOption {
                    type = types.bool;
                    default = false;
                    description = "Whether to automatically open the specified port in the firewall.";
                };
                trustInterface = mkOption {
                    type = types.bool;
                    default = false;
                    description = "Whether to automatically trust the specified interface in the firewall.";
                };
                hostName = mkOption {
                    type = with types; nullOr nonEmptyStr;
                    default = null;
                    description = "The hostname for this device; defaults to `config.networking.hostName'.";
                };
                useUUID = mkOption {
                    type = types.bool;
                    default = false;
                    description = "Use a new UUID as the hostname on every boot; enables `config.services.tailscale.api.ephemeral' by default.";
                };
                deleteHostBeforeAuth = mkOption {
                    type = types.bool;
                    default = false;
                    description = ''
                        Delete the hostname from the tailnet before authentication, if it exists.
                        Does nothing if already authenticated, as determined by the existence of the path from `config.services.tailscale.authenticationConfirmationFile'.
                    '';
                };
                strictReversePathFiltering = mkOption {
                    type = types.bool;
                    default = true;
                    description = "Whether to enable strict reverse path filtering.";
                };
                authkey = mkOption {
                    type = types.nullOr types.nonEmptyStr;
                    default = null;
                    description = ''
                        Authentication key.

                        Warning: Consider using authfile instead if you do not
                        want to store the key in the world-readable Nix store.
                    '';
                };
                authfile = mkOption {
                    example = "/private/tailscale_auth_key";
                    type = with types; nullOr nonEmptyStr;
                    default = null;
                    description = "File with authentication key.";
                };
                api.key = mkOption {
                    type = types.nullOr types.nonEmptyStr;
                    default = null;
                    description = ''
                        API key.

                        Warning: Consider using api.file instead if you do not
                        want to store the key in the world-readable Nix store.
                    '';
                };
                api.file = mkOption {
                    example = "/private/tailscale_api_key";
                    type = with types; nullOr nonEmptyStr;
                    default = null;
                    description = "File with API key.";
                };
                api.tags = mkOption {
                    example = [ "relay" "server" ];
                    type = types.listOf types.nonEmptyStr;
                    default = [ ];
                    description = "Tags to be used when creating new auth keys.";
                };
                api.reusable = mkOption {
                    type = types.bool;
                    default = false;
                    description = "Create a reusable auth key.";
                };
                api.ephemeral = mkOption {
                    type = with types; nullOr bool;
                    default = null;
                    description = "Create an ephemeral auth key; is enabled by default by `config.services.tailscale.useUUID'.";
                };
                api.preauthorized = mkOption {
                    type = types.bool;
                    default = true;
                    description = "Create a pre-authorized auth key.";
                };
                api.domain = mkOption {
                    type = with types; nullOr nonEmptyStr;
                    default = null;
                    description = "Your tailscale domain.";
                };
                state.text = mkOption {
                    type = types.nullOr types.lines;
                    default = null;
                    description = ''
                        The state of tailscale, written to /var/lib/tailscale/tailscaled.state

                        Warning: Consider using state.{file|dir} instead if you do not
                        want to store the state in the world-readable Nix store.
                    '';
                };
                state.file = mkOption {
                    example = "/private/tailscale/tailscaled.state";
                    type = with types; nullOr nonEmptyStr;
                    default = null;
                    description = "File with the state of tailscale.";
                };
                state.dir = mkOption {
                    example = "/private/tailscale";
                    type = with types; nullOr nonEmptyStr;
                    default = null;
                    description = "Directory with the state file (tailscaled.state) of tailscale.";
                };
                magicDNS.enable = mkEnableOption "MagicDNS";
                magicDNS.searchDomains = mkOption {
                    type = types.listOf types.nonEmptyStr;
                    default = [ ];
                    description = "MagicDNS search domains.";
                };
                magicDNS.nameservers = mkOption {
                    type = types.listOf types.nonEmptyStr;
                    default = [ ];
                    description = "MagicDNS nameservers.";
                };
                acceptDNS = mkOption {
                    type = types.bool;
                    default = true;
                    description = "Whether this tailscale instance will use the preconfigured DNS servers on the tailscale admin page.";
                };
                routes.accept = mkOption {
                    type = with types; nullOr bool;
                    default = null;
                    description = "Use subnet routers; enabled by default if `config.services.tailscale.routes.advertise' is null.";
                };
                routes.advertise = mkOption {
                    type = with types; nullOr nonEmptyStr;
                    default = null;
                    description = "Start tailscale as a subnet router with the specified subnets.";
                };
                exitNode.advertise = mkOption {
                    type = types.bool;
                    default = false;
                    description = "Whether this tailscale instance will used as an exit node.";
                };
                exitNode.ip = mkOption {
                    type = with types; nullOr nonEmptyStr;
                    default = null;
                    description = "The exit node, as an ip address, to be used with this device.";
                };
                exitNode.hostName = mkOption {
                    type = with types; nullOr nonEmptyStr;
                    default = null;
                    description = "The exit node, as a hostname, to be used with this device; requires an api key provided via `config.services.tailscale.api.{key|file}'.";
                };
                exitNode.allowLANAccess = mkOption {
                    type = types.bool;
                    default = false;
                    description = "Allow direct access to your local network when traffic is routed via an exit node.";
                };
                extraConfig = mkOption {
                    type = types.attrs;
                    default = { };
                    description = "An attribute set of options and values; if an option is a single character, a single dash will be prepended, otherwise two.";
                };
                authenticationConfirmationFile = mkOption {
                    type = types.nonEmptyStr;
                    example = "/private/tailscale/tailscaled.authenticated";
                    default = "/var/lib/tailscale/tailscale.authenticated";
                    description = "Path to the `tailscale.authenticated' file.";
                };
            };
            guix = {
                enable = mkEnableOption "GNU Guix package manager";
                package = mkOption {
                    type = types.package;
                    default = pkgs.guix;
                    defaultText = "pkgs.guix";
                    description = "Package that contains the guix binary and initial store.";
                };
            };
        };
    };
    imports = [ ./var ];
    config = mkMerge [
        { _module.args.variables = config.variables; }
        (let cfg = config.programs.mosh; in mkIf cfg.enable {
            networking.firewall.allowedUDPPortRanges = optional cfg.openFirewall { from = 60000; to = 61000; };
        })
        (let cfg = config.services.guix; in mkIf cfg.enable {
            users = {
                extraUsers = lib.fold (a: b: a // b) {} (builtins.map buildGuixUser (lib.range 1 10));
                extraGroups.guixbuild = {name = "guixbuild";};
            };
            systemd.services.guix-daemon = {
                enable = true;
                description = "Build daemon for GNU Guix";
                serviceConfig = {
                    ExecStart="/var/guix/profiles/per-user/root/current-guix/bin/guix-daemon --build-users-group=guixbuild";
                    Environment="GUIX_LOCPATH=/var/guix/profiles/per-user/root/guix-profile/lib/locale";
                    RemainAfterExit="yes";

                    # See <https://lists.gnu.org/archive/html/guix-devel/2016-04/msg00608.html>.
                    # Some package builds (for example, go@1.8.1) may require even more than
                    # 1024 tasks.
                    TasksMax="8192";
                };
                wantedBy = [ "multi-user.target" ];
            };
            system.activationScripts.guix = ''
                # copy initial /gnu/store
                if [ ! -d /gnu/store ]
                then
                    mkdir -p /gnu
                    cp -ra ${cfg.package.store}/gnu/store /gnu/
                fi

                # copy initial /var/guix content
                if [ ! -d /var/guix ]
                then
                    mkdir -p /var
                    cp -ra ${cfg.package.var}/var/guix /var/
                fi

                # root profile
                if [ ! -d ~root/.config/guix ]
                then
                    mkdir -p ~root/.config/guix
                    ln -sf /var/guix/profiles/per-user/root/current-guix \
                    ~root/.config/guix/current
                fi

                # authorize substitutes
                GUIX_PROFILE="`echo ~root`/.config/guix/current"; source $GUIX_PROFILE/etc/profile
                guix archive --authorize < ~root/.config/guix/current/share/guix/ci.guix.info.pub
            '';

            environment.shellInit = ''
                # Make the Guix command available to users
                export PATH="/var/guix/profiles/per-user/root/current-guix/bin:$PATH"

                export GUIX_LOCPATH="$HOME/.guix-profile/lib/locale"
                export PATH="$HOME/.guix-profile/bin:$PATH"
                export INFOPATH="$HOME/.guix-profile/share/info:$INFOPATH"
            '';
        })
        (let cfg = config.services.tailscale; in mkIf cfg.enable {
            assertions = flatten [
                (optional ((count (state: state != null) (with cfg.state; [ text file dir ])) > 1)
                          "Sorry; only one of `config.services.tailscale.state.{text|file|dir}' may be set!")
                (optional ((cfg.exitNode.ip != null) && (cfg.exitNode.hostName != null))
                          "Sorry; only one of `config.services.tailscale.exitNode.{ip|hostName}' may be set!")
                (optional ((cfg.exitNode.hostName != null) && (cfg.api.key == null) && (cfg.api.file == null))
                          "Sorry; `config.services.tailscale.api.{key|file}' must be set when using `config.services.tailscale.exitNode.hostName'!")
                (optional ((count (auth: auth != null) (with cfg; [ authkey authfile api.key api.file ])) > 1)
                          "Sorry; only one of `config.services.tailscale.{authkey|authfile|api.key|api.file}' may be set!")
                (optional ((cfg.api.domain == null) && ((cfg.api.key != null) || (cfg.api.file != null)))
                          "Sorry; `config.services.tailscale.api.domain' must be set when using `config.services.tailscale.api.{key|file}'!")
            ];
            warnings = flatten [
                (optional ((cfg.api.key != null) || (cfg.api.file != null))
                          "To users who wipe their root directories: persist your `config.services.tailscale.authenticationConfirmationFile'!")
                (optional (cfg.exitNode.advertise && cfg.acceptDNS)
                          "Advertising this device as an exit node and accepting the preconfigured DNS servers on the tailscale admin page at the same time may result in this device attempting to use itself as a DNS server.")

                # TODO: Why is this causing an infinite recursion error?
                # (optional (((isBool cfg.routes.accept) && cfg.routes.accept) && (cfg.routes.advertise != null))
                #           "Advertising this device as a subnet router and accepting the preconfigured subnet routes on the tailscale admin page at the same time may result in this device #TODO")

            ];
            services.tailscale = {
                api.ephemeral = if (cfg.api.ephemeral == null) then config.services.tailscale.useUUID else cfg.api.ephemeral;
                hostName = if (cfg.hostName == null) then config.networking.hostName else cfg.hostName;
                routes.accept = if (cfg.routes.accept == null) then (cfg.routes.advertise == null) else cfg.routes.accept;
            };
            environment.vars = let
                nullText = cfg.state.text != null;
                nullFile = cfg.state.file != null;
                nullDir = cfg.state.dir != null;
            in optionalAttrs (nullText || nullFile || nullDir) {
                "lib/tailscale/tailscaled.state" = mkIf (nullText || nullFile) {
                    ${if nullText then "text" else "source"} = if (nullText) then cfg.state.text else cfg.state.file;
                };
                "lib/tailscale" = mkIf nullDir { source = cfg.state.dir; };
            };
            networking = {
                nameservers = optionals cfg.magicDNS.enable (flatten [ cfg.magicDNS.nameservers "100.100.100.100" ]);
                search = optionals cfg.magicDNS.enable cfg.magicDNS.searchDomains;
                firewall = {
                    ${if cfg.strictReversePathFiltering then null else "checkReversePath"} = "loose";
                    trustedInterfaces = optional cfg.trustInterface cfg.interfaceName;
                    allowedUDPPorts = optional cfg.openFirewall cfg.port;
                };
            };
            systemd.services.tailscale-autoconnect = let
                api_and_no_authConFile = ((cfg.api.key != null) || (cfg.api.file != null)) && (! pathExists cfg.authenticationConfirmationFile);
            in mkIf cfg.autoconnect {
                description = "Automatic connection to Tailscale";

                # make sure tailscale is running before trying to connect to tailscale
                after = [ "network-pre.target" "tailscale.service" ];
                wants = [ "network-pre.target" "tailscale.service" ];
                wantedBy = [ "multi-user.target" ];

                environment = mkIf api_and_no_authConFile {
                TAILSCALE_APIKEY = if (cfg.api.key != null) then cfg.api.key else (readFile cfg.api.file);
                };

                # set this service as a oneshot job
                serviceConfig = {
                    Type = "oneshot";
                    ExecStart = let
                        extraConfig = mapAttrsToList (opt: val: let
                        value = optionalString (! isBool val) " ${toString val}";
                        in (if ((stringLength opt) == 1) then "-" else "--") + opt + value) cfg.extraConfig;
                    in ''
                        # wait for tailscaled to settle
                        sleep 2

                        # check if we are already authenticated to tailscale
                        echo "Waiting for tailscale.service start completion ..."
                        status="$(${cfg.package}/bin/tailscale status -json | ${pkgs.jq}/bin/jq -r .BackendState)"
                        if [ $status = "Running" ]; then # if so, then do nothing
                            echo "Already authenticated to Tailscale, exiting."
                            exit 0
                        fi

                        # Delete host from tailnet if:
                        # * `config.services.tailscale.deleteHostBeforeAuth' is enabled
                        # * `config.services.tailscale.api.{key|file}' is not null
                        # * path at `config.services.tailscale.authenticationConfirmationFile` does not exist
                        ${optionalString (cfg.deleteHostBeforeAuth && api_and_no_authConFile) ''${pkgs.tailapi}/bin/tailapi --domain ${cfg.api.domain} \
                                                                                                                            --recreate-response \
                                                                                                                            --devices ${cfg.hostName} \
                                                                                                                            delete \
                                                                                                                            --do-not-prompt &> /dev/null''}

                        # otherwise authenticate with tailscale
                        echo "Authenticating with Tailscale ..."
                        ${cfg.package}/bin/tailscale up --hostname ${if cfg.useUUID then "$(${pkgs.util-linux}/bin/uuidgen)" else cfg.hostName} \
                        ${optionalString cfg.acceptDNS "--accept-dns \\"}
                        ${optionalString cfg.routes.accept "--accept-routes \\"}
                        ${optionalString (cfg.routes.advertise != null) "--advertise-routes ${cfg.routes.advertise} \\"}
                        ${optionalString cfg.exitNode.advertise "--advertise-exit-node \\"}
                        ${optionalString (cfg.exitNode.ip != null) "--exit-node ${cfg.exitNode.ip} \\"}
                        ${optionalString (cfg.exitNode.hostName != null) ''--exit-node $(${pkgs.tailapi}/bin/tailapi --domain ${cfg.api.domain} \
                                                                           --recreate-response \
                                                                           --devices ${cfg.exitNode.hostName} \
                                                                           ip -f4) \''}
                        ${optionalString (((cfg.exitNode.ip != null) || (cfg.exitNode.hostName != null)) && cfg.exitNode.allowLANAccess)
                                         "--exit-node-allow-lan-access \\"}

                        ${concatStringsSep " " (mapAttrsToList (n: v: let
                            opt = (if ((stringLength n) == 1) then "-" else "--") + n;
                        in "${opt} ${v}") extraConfig)} \

                        ${optionalString (cfg.authkey != null) "--authkey ${cfg.authkey} \\"}
                        ${optionalString (cfg.authfile != null) "--authkey ${readFile cfg.authfile} \\"}
                        ${optionalString api_and_no_authConFile ''--authkey $(${pkgs.tailapi}/bin/tailapi --domain ${cfg.api.domain} \
                                                                                                          --recreate-response \
                                                                                                          create \
                                                                                                          ${optionalString cfg.api.reusable "--reusable \\"}
                                                                                                          ${optionalString cfg.api.ephemeral "--ephemeral \\"}
                                                                                                          ${optionalString cfg.api.reusable "--preauthorized \\"}
                                                                                                          ${optionalString (cfg.api.tags != null)
                                                                                                                           (concatStringsSep " " cfg.api.tags)} \
                                                                                                          --just-key)''}

                        ${optionalString ((cfg.state.file != null) && (! pathExists cfg.state.file)) "cp /var/lib/tailscale/tailscaled.state ${cfg.state.file}"}
                        ${optionalString ((cfg.state.dir != null) && ((! pathExists cfg.state.dir) || ((length (attrNames (readDir cfg.state.dir))) == 0)))
                                         "${pkgs.rsync}/bin/rsync -avvczz /var/lib/tailscale/ ${cfg.state.dir}/"}

                        touch ${cfg.authenticationConfirmationFile}
                    '';
                };
            };
        })
    ];
}
