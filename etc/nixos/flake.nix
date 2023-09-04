{
  description = "shadowrylander's Flake!";
  inputs = rec {
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };

    bundle.url = "git+file:///home/shadowrylander/shadowrylander/bundle";
    valiant.follows = "bundle/valiant";
    nixpkgs.follows = "bundle/nixpkgs";
    sylveon.url = "git+file:///home/shadowrylander/shadowrylander/sylveon";

    home-manager.url = "github:nix-community/home-manager";
    kitty-themes = {
      url = "github:dexpota/kitty-themes";
      flake = false;
    };

    hardware.url = "github:nixos/nixos-hardware";
    impermanence.url = "github:nix-community/impermanence";

    home-manager-config = {
      url = "path:home-manager";
      inputs.user.follows = "";
    };
  };
  outputs = inputs@{ self, nixpkgs, flake-utils, bundle, ... }:
    with builtins;
    with bundle.lib;
    with flake-utils.lib;
    let

      lib = bundle.lib.extend (final: prev: {
        j = prev.bundle.extend (new: old:
          with final;
          with new;
          makeExtensible (self: rec {

            # TODO: Is this necessary?
            mntConvert = dir:
              let mntDir = "/mnt/" + dir;
              in if (pathExists mntDir) then mntDir else dir;

            attrs = {
              config = {
                nix = let
                  MG = size:
                    let mg = stringToCharacters size;
                    in toString ((toInt (elemAt mg 0))
                      * (if (elemAt mg 1 == "M") then 1 else 1024) * 1024
                      * 1024);
                in ''
                  # extra-substituters = https://cache.nixos.org/ https://nix-community.cachix.org
                  trusted-substituters = https://cache.nixos.org/
                  # extra-trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
                  trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
                  keep-derivations = true
                  keep-outputs = true
                  extra-experimental-features = nix-command flakes impure-derivations recursive-nix
                  accept-flake-config = true
                  show-trace = true
                  fallback = true
                  auto-optimise-store = true
                  builders-use-substitutes = true
                  cores = 0
                  flake-registry = https://raw.githubusercontent.com/syvlorg/flake-registry/master/flake-registry.json
                  allow-unsafe-native-code-during-evaluation = true
                  min-free = 262144000
                  max-free = 1073741824
                  min-free = ${MG "250M"}
                  max-free = ${MG "1G"}
                '';
                services = rec {
                  mkBase = User: {
                    enable = true;
                    serviceConfig = rec {
                      Restart = "on-failure";
                      inherit User;
                      Group = User;
                      Environment = [ "PATH=/run/wrappers/bin:$PATH" ];
                    };
                    wantedBy = [ "multi-user.target" ];
                  };
                  base = mkBase users.primary;
                  mkdir = path:
                    "/run/current-system/sw/bin/mkdir -p ${path} &> /dev/null";
                };
              };

              users = fromJSON ''
                {
                    "primary": "shadowrylander",
                    "secondary": "frost",
                    "nightingale": "curtis"
                }
              '';
              usernames = attrValues users;
              designations = attrNames users;

              excludedUsers = { root = "root"; };
              excludedUsernames = attrValues excludedUsers;
              excludedDesignations = attrNames excludedUsers;

              allUsers = recursiveUpdate users excludedUsers;
              allUsernames = attrValues allUsers;
              allDesignations = attrNames allUsers;

              homes = fromJSON ''
                {
                    "primary": "/home/shadowrylander",
                    "secondary": "/home/frost",
                    "nightingale": "/home/curtis"
                }
              '';
              excludedHomes = { root = "/root"; };
              allHomes = recursiveUpdate homes excludedHomes;

              datasets = {
                backup = [ "system/persist" "virt" "omniverse" users.primary ];
              };
              ssh.keys = rec {
                "id_rsa.bak" =
                  "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDlwBJ7E2qeqw9kMW19indbeLdnEKs/Yrhn9HE0c/gZDzvXYBPQYyf5xr9I9kYxWcHlqp7XEI0LVT4DCA/mgemQtM8ulc1mxwekKtk64uWRi5wLi1E17NWKJfXWRn8XZejwi0iJa0twwVE8m8G2AuFOCSa86sYD3x5X5W+7spAuNET7kl0DLueUHu1u31c7HE1ciV2tIn/f60/bbgEJm9MPcRVZkRxkp+bouaZ1cjWRYDhvyJS30DRhBYtIIort2XVAshQs2Y58oKeCDnjt0gxotfqqWlt4nTQzKtbSN2M6/M+clFQBdT1oUJqpTUJbVxK8+xSEOJcBubupTj0USpmftDf/3WMoMwq+hNEc9C0EN1BYtKk68QWhAz8NROvnx7h6y3UKejhQOg0ueNZggmeNJLbebEs46QmA92khO8zc2pfBRsEa5yP0IgdvWpruTZ1QwjqhGQqGnCw3Oli1PK+5zgT2vXy5yHl3f3duPq8h+LOc+lSBbi2jjkC0gwTQDDDNyzFZ+U9xF7fCmL3V8DCEeO/4HqVxmLJir2TVEDo/3Ug/Q22Yp7P2EZrI2pikZIyBJc5aZJO3d7nGoDB/1BJp9Qm82wvyEpjiOnxHsL4osUqrf401XbiwNqpFkVUoRZkwGraJnrlsYkdHS2Mrrny9sr+PtgZhTjqIuW8z6iVIfQ== titaniumfiles@outlook.com";
                "id_ed25519.bak" =
                  "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIO8NzKV52dRBAir8ARoFJX/xQDVCNup6xe1ddX1YVXSO sylvorg@syvl.org";
                jeet_ray_ecdsa =
                  "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBGsRy6rLDzmLNISdWahFLGDo+ZZLbndj6k8Q8MUQum/mPAzy8lsAQz/0XiicJz7LlM74tWGDYSJG1Ay2Iyc/ew4= jeet.ray@syvl.org";
                jeet_ray_ed25519 =
                  "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICor+WXoAypnk5rkgTljAN6kk8olvKWqtnmGWVuQu8z9 jeet.ray@syvl.org";
                jeet_ray_rsa =
                  "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCs4zqEt/Fkjw0LVQTwJXlovmnqqGWC4UOVPvoLDvo0JD6WeVBDi4cFPX2mpNJYmYJsBLDXeUq5XrQ1ST3BkfVdspsragnD7O92tTEf3/VHfIC1L165pnB08FXQrtIjyLL7Ry4dloUGBYKLnHOtnXlpefKMQzRYUacc7Tr1o2wv+XRoDW9h+qDqJz1O61N68JFLgJWD3/nUkm8siTg1OLvqO9ATp+UgP/Lb08E6HfqYOiD8H+1ZJjz78mo5oZatknvgy8uJJPqEX7/aRM61YA9TG+tw/sf6wlrDtUQUik8Y4k1DLmkhE15wcgq/HF2Rqka/acA9GxA5smNGyjs6CS+H jeet.ray@syvl.org";
                shadowrylander_ecdsa =
                  "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBNlVuY9reRuMloYvecJHHsOYkAPDyQwELOI3kfibslIKI5hY+o1jx5yVyAUomHynP6wulm5aziNc5kWdsRE9BE8= shadowrylander@syvl.org";
                shadowrylander_ed25519 =
                  "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINZ2FCMbnetAnDZ63Wzct+O3MYhtO9+BedATbtiHI9BT shadowrylander@syvl.org";
                shadowrylander_rsa =
                  "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDP8ifT/3d6L2MzZPoTh0bUjQUXuPKfPP8Tp03E5En2x+iKhv+J/U0z0xk7IdPZ4qEO+ZWI5xWbwVkDdnRnJ/5HgA0/ZwmO5Zpj3llSr4dJMUVSSyO23fFIL2WqOpHyQDeexJWMxbU5SmIi+c855VwewCbGDcPnmDo0XgR/u4LRF2pwYGNGFtJ2/GICEIob/2w0ICwi7TMUEkDbUFcP5web81OzsNu80M60VaNl870uT1rwBeKuW7CXFtImYytZ0mOc5LC6d7ugkFS1zAbLOWjt3PJ8Op2MH9ncBj5jCsIlA/OqI72jKwEPOl8evYqWeEOzlVxA7/AkRj7haQqFE8r/ shadowrylander@syvl.org";
                id_rsa = shadowrylander_rsa;
                id_ed25519 = jeet_ray_ed25519;
                id_ecdsa = jeet_ray_ecdsa;
              };
              fileSystems = {
                base = {
                  fsType = "zfs";
                  options =
                    [ "defaults" "x-systemd.device-timeout=5" "nofail" ];
                };
                supported = [
                  "zfs"
                  "xfs"
                  "btrfs"
                  "ext4"
                  "fat"
                  "vfat"

                  # TODO
                  # "bcachefs"

                ];
              };
              commands = {
                rebuild = "nixos-rebuild --show-trace";
                install = "nixos-install --show-trace";
              };
            };
          }));
      });

      hosts = {
        bastiodon = { config, ... }: {
          imports = toList bundle.devices.rpi3;
          services.tailscale = {
            exitNode.advertise = true;
            acceptDNS = !config.services.tailscale.exitNode.advertise;
          };
          networking.hostId = "63c69cb4";
          variables.relay = true;
        };
        sandshrew = { config, lib, ... }: {
          imports = toList inputs.hardware.microsoft-surface;
          variables.encrypted = true;
          networking = {
            hostId = "7c56f71e";
            networkmanager.extraConfig = ''
              [connection]
              wifi.powersave = 2

              [device]
              wifi.scan-rand-mac-address=false
            '';
          };

          # TODO
          # services.surface-dtx-daemon.detach = mkForce ''
          #     #!/usr/bin/env sh
          #     for usb in $(ls /dev/disk/by-id).split("\n"):
          #         if usb and usb[:4] == "usb-":
          #             for mnt in $(mount).split("\n"):
          #                 if mnt and usb in mnt:
          #                     umount @(mnt.split()[2])
          # '';

        };
        siluam = { config, lib, ... }: {
          imports = toList inputs.hardware.pine64-pinebook-pro;
          networking.hostId = "3f20fe10";
          swapDevices = [{ device = "/dev/mmcblk2p2"; }];
        };
        murasame = { config, ... }: { networking.hostId = "94c04107"; };
        argus = { config, pkgs, lib, ... }: {
          imports = toList bundle.devices.linode;
          networking.hostId = "fb7240c5";
          variables.relay = true;
        };
        yggdrasil = { config, ... }: {
          imports = toList bundle.devices.rip4;
          networking.hostId = "55db4181";
          variables.zfs = false;
        };
        enterprise = { config, ... }: {
          imports = toList bundle.devices.repi4;
          networking.hostId = "d7c663c7";
        };
      };
      nixosModules = rec {
        baseModules = mapAttrNames (n: v: "base-" + n) (bundle.fold.set [
          bundle.nixosModules
          {
            inherit (home-manager.nixosModules) home-manager;
            inherit (impermanence.nixosModules) impermanence;
            cachix = import ./cachix.nix;
          }
          {
            options = args@{ config, options, pkgs, ... }: {
              options = {
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
                      description =
                        "Whether to automatically open the specified port in the firewall.";
                    };
                    trustInterface = mkOption {
                      type = types.bool;
                      default = false;
                      description =
                        "Whether to automatically trust the specified interface in the firewall.";
                    };
                    hostName = mkOption {
                      type = with types; nullOr nonEmptyStr;
                      default = null;
                      description =
                        "The hostname for this device; defaults to `config.networking.hostName'.";
                    };
                    useUUID = mkOption {
                      type = types.bool;
                      default = false;
                      description =
                        "Use a new UUID as the hostname on every boot; enables `config.services.tailscale.api.ephemeral' by default.";
                    };
                    deleteHostBeforeAuth = mkOption {
                      type = types.bool;
                      default = false;
                      description = ''
                        Delete the hostname from the tailnet before authentication, if it exists.
                        Does nothing if already authenticated.
                      '';
                    };
                    strictReversePathFiltering = mkOption {
                      type = types.bool;
                      default = true;
                      description =
                        "Whether to enable strict reverse path filtering.";
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
                      description =
                        "Tags to be used when creating new auth keys.";
                    };
                    api.reusable = mkOption {
                      type = types.bool;
                      default = false;
                      description = "Create a reusable auth key.";
                    };
                    api.ephemeral = mkOption {
                      type = with types; nullOr bool;
                      default = null;
                      description =
                        "Create an ephemeral auth key; is enabled by default by `config.services.tailscale.useUUID'.";
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
                      description =
                        "Directory with the state file (tailscaled.state) of tailscale.";
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
                      description =
                        "Whether this tailscale instance will use the preconfigured DNS servers on the tailscale admin page.";
                    };
                    routes.accept = mkOption {
                      type = with types; nullOr bool;
                      default = null;
                      description =
                        "Use subnet routers; enabled by default if `config.services.tailscale.routes.advertise' is null.";
                    };
                    routes.advertise = mkOption {
                      type = with types; nullOr nonEmptyStr;
                      default = null;
                      description =
                        "Start tailscale as a subnet router with the specified subnets.";
                    };
                    exitNode.advertise = mkOption {
                      type = types.bool;
                      default = false;
                      description =
                        "Whether this tailscale instance will used as an exit node.";
                    };
                    exitNode.ip = mkOption {
                      type = with types; nullOr nonEmptyStr;
                      default = null;
                      description =
                        "The exit node, as an ip address, to be used with this device.";
                    };
                    exitNode.hostName = mkOption {
                      type = with types; nullOr nonEmptyStr;
                      default = null;
                      description =
                        "The exit node, as a hostname, to be used with this device; requires an api key provided via `config.services.tailscale.api.{key|file}'.";
                    };
                    exitNode.allowLANAccess = mkOption {
                      type = types.bool;
                      default = false;
                      description =
                        "Allow direct access to your local network when traffic is routed via an exit node.";
                    };
                    extraConfig = mkOption {
                      type = types.attrs;
                      default = { };
                      description =
                        "An attribute set of options and values; if an option is a single character, a single dash will be prepended, otherwise two.";
                    };
                  };
                };
              };
              config = mkMerge [
                (let cfg = config.services.tailscale;
                in mkIf cfg.enable {
                  assertions = flatten [
                    (optional ((count (state: state != null)
                      (with cfg.state; [ text file dir ])) > 1)
                      "Sorry; only one of `config.services.tailscale.state.{text|file|dir}' may be set!")
                    (optional ((cfg.exitNode.ip != null)
                      && (cfg.exitNode.hostName != null))
                      "Sorry; only one of `config.services.tailscale.exitNode.{ip|hostName}' may be set!")
                    (optional ((cfg.exitNode.hostName != null)
                      && (cfg.api.key == null) && (cfg.api.file == null))
                      "Sorry; `config.services.tailscale.api.{key|file}' must be set when using `config.services.tailscale.exitNode.hostName'!")
                    (optional ((count (auth: auth != null)
                      (with cfg; [ authkey authfile api.key api.file ])) > 1)
                      "Sorry; only one of `config.services.tailscale.{authkey|authfile|api.key|api.file}' may be set!")
                    (optional ((cfg.api.domain == null)
                      && ((cfg.api.key != null) || (cfg.api.file != null)))
                      "Sorry; `config.services.tailscale.api.domain' must be set when using `config.services.tailscale.api.{key|file}'!")
                  ];
                  warnings = flatten [
                    (optional (cfg.exitNode.advertise && cfg.acceptDNS)
                      "Advertising this device as an exit node and accepting the preconfigured DNS servers on the tailscale admin page at the same time may result in this device attempting to use itself as a DNS server.")

                    # TODO: Why is this causing an infinite recursion error?
                    # (optional (((isBool cfg.routes.accept) && cfg.routes.accept) && (cfg.routes.advertise != null))
                    #           "Advertising this device as a subnet router and accepting the preconfigured subnet routes on the tailscale admin page at the same time may result in this device #TODO")

                  ];
                  services.tailscale = {
                    api.ephemeral = if (cfg.api.ephemeral == null) then
                      config.services.tailscale.useUUID
                    else
                      cfg.api.ephemeral;
                    hostName = if (cfg.hostName == null) then
                      config.networking.hostName
                    else
                      cfg.hostName;
                    routes.accept = if (cfg.routes.accept == null) then
                      (cfg.routes.advertise == null)
                    else
                      cfg.routes.accept;
                  };
                  environment.vars = let
                    nullText = cfg.state.text != null;
                    nullFile = cfg.state.file != null;
                    nullDir = cfg.state.dir != null;
                  in optionalAttrs (nullText || nullFile || nullDir) {
                    "lib/tailscale/tailscaled.state" =
                      mkIf (nullText || nullFile) {
                        ${if nullText then "text" else "source"} =
                          if (nullText) then cfg.state.text else cfg.state.file;
                      };
                    "lib/tailscale" = mkIf nullDir { source = cfg.state.dir; };
                  };
                  networking = {
                    nameservers = optionals cfg.magicDNS.enable
                      (flatten [ cfg.magicDNS.nameservers "100.100.100.100" ]);
                    search =
                      optionals cfg.magicDNS.enable cfg.magicDNS.searchDomains;
                    firewall = {
                      ${
                        if cfg.strictReversePathFiltering then
                          null
                        else
                          "checkReversePath"
                      } = "loose";
                      trustedInterfaces =
                        optional cfg.trustInterface cfg.interfaceName;
                      allowedUDPPorts = optional cfg.openFirewall cfg.port;
                    };
                  };
                  systemd.services.tailscale-autoconnect =
                    mkIf cfg.autoconnect {
                      description = "Automatic connection to Tailscale";

                      # make sure tailscale is running before trying to connect to tailscale
                      after = [ "network-pre.target" "tailscale.service" ];
                      wants = [ "network-pre.target" "tailscale.service" ];
                      wantedBy = [ "multi-user.target" ];

                      environment.TAILSCALE_APIKEY =
                        if (cfg.api.key != null) then
                          cfg.api.key
                        else
                          (readFile cfg.api.file);

                      # set this service as a oneshot job
                      serviceConfig = {
                        Type = "oneshot";
                        ExecStart = let
                          extraConfig = mapAttrsToList (opt: val:
                            let
                              value = optionalString (!(isBool val))
                                " ${toString val}";
                            in (if ((stringLength opt) == 1) then "-" else "--")
                            + opt + value) cfg.extraConfig;
                          connect = authenticating: ''
                            # otherwise connect to ${
                              optionalString authenticating
                              "and authenticate with "
                            }tailscale
                            echo "Connecting to ${
                              optionalString authenticating
                              "and authenticating with "
                            }Tailscale ..."
                            ${cfg.package}/bin/tailscale up --hostname ${
                              if cfg.useUUID then
                                "$(${pkgs.util-linux}/bin/uuidgen)"
                              else
                                cfg.hostName
                            } \
                            ${optionalString cfg.acceptDNS "--accept-dns \\"}
                            ${optionalString cfg.routes.accept
                            "--accept-routes \\"}
                            ${optionalString (cfg.routes.advertise != null)
                            "--advertise-routes ${cfg.routes.advertise} \\"}
                            ${optionalString cfg.exitNode.advertise
                            "--advertise-exit-node \\"}
                            ${optionalString (cfg.exitNode.ip != null)
                            "--exit-node ${cfg.exitNode.ip} \\"}
                            ${optionalString (cfg.exitNode.hostName != null) ''
                              --exit-node $(${pkgs.tailapi}/bin/tailapi --domain ${cfg.api.domain} \
                                                                                                                                 --recreate-response \
                                                                                                                                 --devices ${cfg.exitNode.hostName} \
                                                                                                                                 ip -f4) \''}
                            ${optionalString (((cfg.exitNode.ip != null)
                              || (cfg.exitNode.hostName != null))
                              && cfg.exitNode.allowLANAccess)
                            "--exit-node-allow-lan-access \\"}

                            ${
                              toString (mapAttrsToList (n: v:
                                let
                                  opt = (if ((stringLength n) == 1) then
                                    "-"
                                  else
                                    "--") + n;
                                in "${opt} ${v}") extraConfig)
                            } \

                            ${optionalString
                            (authenticating && (cfg.authkey != null))
                            "--authkey ${cfg.authkey} \\"}
                            ${optionalString
                            (authenticating && (cfg.authfile != null))
                            "--authkey ${readFile cfg.authfile} \\"}
                            ${optionalString authenticating ''
                              --authkey $(${pkgs.tailapi}/bin/tailapi \
                              --domain ${cfg.api.domain} \
                              --recreate-response \
                              create \
                              ${optionalString cfg.api.reusable "--reusable \\"}
                              ${optionalString cfg.api.ephemeral
                              "--ephemeral \\"}
                              ${optionalString cfg.api.reusable
                              "--preauthorized \\"}
                              ${
                                optionalString (cfg.api.tags != null)
                                (toString cfg.api.tags)
                              } \
                              --just-key)''}
                          '';
                        in ''
                          # wait for tailscaled to settle
                          sleep 2

                          # check if we are already connected to tailscale
                          echo "Waiting for tailscale.service start completion ..."
                          status="$(${cfg.package}/bin/tailscale status -json | ${pkgs.jq}/bin/jq -r .BackendState)"
                          if [ $status = "Running" ]; then # if so, then do nothing
                              echo "Already connected to Tailscale, exiting."
                              exit 0
                          fi

                          # Delete host from tailnet if:
                          # * `config.services.tailscale.deleteHostBeforeAuth' is enabled
                          # * `config.services.tailscale.api.{key|file}' is not null
                          # * tailscale is not authenticated
                          if [ $status = "NeedsLogin" ]; then
                              ${
                                if cfg.deleteHostBeforeAuth then ''
                                  ${pkgs.coreutils}/bin/cat <<EOF
                                  Because `config.services.tailscale.deleteHostBeforeAuth' has been enabled,
                                  any devices with hostname "${config.networking.hostName}" will be deleted before authentication.
                                  EOF'' else ''
                                    ${pkgs.coreutils}/bin/cat <<EOF
                                    Because `config.services.tailscale.deleteHostBeforeAuth' has not been enabled,
                                    any devices with hostname "${config.networking.hostName}" will not be deleted before authentication.
                                    EOF
                                  ''
                              }
                              ${
                                optionalString cfg.deleteHostBeforeAuth ''
                                  ${pkgs.tailapi}/bin/tailapi --domain ${cfg.api.domain} \
                                                              --recreate-response \
                                                              --devices ${cfg.hostName} \
                                                              delete \
                                                              --do-not-prompt &> /dev/null && \
                                                              echo Successfully deleted device of hostname \"${config.networking.hostName}\"!"''
                              }
                          fi

                          if [ $status = "NeedsLogin" ]; then
                              ${connect true}
                          else
                              ${connect false}
                          fi

                          ${optionalString ((cfg.state.file != null)
                            && (!(pathExists cfg.state.file)))
                          "cp /var/lib/tailscale/tailscaled.state ${cfg.state.file}"}
                          ${optionalString ((cfg.state.dir != null)
                            && ((!(pathExists cfg.state.dir))
                              || ((length (attrNames (readDir cfg.state.dir)))
                                == 0)))
                          "${pkgs.rsync}/bin/rsync -avvczz /var/lib/tailscale/ ${cfg.state.dir}/"}
                        '';
                      };
                    };
                })
              ];
            };
          }
        ]);
        miniModules = bundle.fold.set [
          baseModules
          (bundle.mapAttrNames (n: v: "mini-" + n) {
            power = args@{ config, ... }: {
              services.logind.lidSwitch = "hybrid-sleep";
              powerManagement = mkMerge [
                config.configs.hardware.powerManagement
                {
                  enable = true;
                  cpuFreqGovernor = mkForce "ondemand";
                }
              ];
            };
            nixpkgs = { config, nixpkgs, ... }: { inherit nixpkgs; };
            boot = args@{ config, pkgs, ... }: {
              boot = {
                kernelPackages = mkDefault pkgs.linuxPackages_xanmod;
                # kernelPackages = mkDefault pkgs.linuxPackages_lqx;
                # kernelPackages = mkDefault pkgs.linuxPackages_zen;
                supportedFilesystems = bundle.attrs.fileSystems.supported;
                initrd = {
                  inherit (config.configs.hardware.boot.initrd)
                    availableKernelModules kernelModules;
                  network.ssh.enable = true;
                  inherit (config.boot) supportedFilesystems;
                  compressor = "${getBin pkgs.zstd}/bin/zstd";
                };
                inherit (config.configs.hardware.boot)
                  kernelModules extraModulePackages;
                extraModprobeConfig = "options kvm_intel_nested=1 ";
                loader = {
                  generic-extlinux-compatible.enable = mkForce false;
                  systemd-boot = {
                    configurationLimit = 25;
                    editor = mkForce false;
                    # enable = mkForce false;
                    enable = mkForce true;
                  };
                  grub = {
                    # enable = mkForce true;
                    enable = mkForce false;
                    efiSupport = true;
                    efiInstallAsRemovable = mkForce false;
                    # efiInstallAsRemovable = mkForce true;
                    # devices = [ "nodev" ];
                    # device = "nodev";
                    device = if config.boot.loader.grub.efiSupport then
                      config.boot.loader.efi.efiSysMountPoint
                    else
                      "/boot";
                    version = 2;
                    useOSProber = true;

                    # TODO: Get more options
                    extraEntries = ''
                      menuentry "Reboot" { reboot }
                      menuentry "Poweroff" { halt }
                    '';
                  };
                  efi = {
                    canTouchEfiVariables = mkForce true;
                    # canTouchEfiVariables = mkForce false;
                    efiSysMountPoint = "/boot/efi";
                  };
                  timeout = 10;

                  # Used for Bedrock Linux
                  # Also causing EFI stuff not to be installed
                  # initScript.enable = mkForce true;

                };
                kernelPatches = flatten [

                  # TODO
                  # (optionals (elem "bcachefs" config.boot.supportedFilesystems) (filter (set: hasInfix "bcachefs" set.name) pkgs.linuxKernel.kernels.linux_testing_bcachefs.kernelPatches))

                  {
                    name = "Enable ZSTD Compression";
                    patch = null;
                    extraConfig = ''
                      RD_ZSTD y
                      KERNEL_ZSTD y
                      KERNEL_XZ n
                    '';
                  }

                ];
              };
            };
            zfs = { config, pkgs, host, ... }:
              mkIf config.variables.zfs
              (let datasets = import ./datasets.nix host;
              in {
                boot = {
                  extraModulePackages = with config.boot.kernelPackages;
                    [ zfsUnstable ];
                  kernelModules = [ "zfs" ];
                  kernelParams = [ "nohibernate" ];
                  loader.grub.zfsSupport = true;
                  initrd = {
                    postDeviceCommands = mkAfter (concatMapStrings (d: ''
                      zfs rollback -r ${d}@blank
                    '') (filter (bundle.has.prefix [
                      "${host}/system/home"
                      "${host}/system/root"
                      # "${host}/system/tmp"
                    ]) (attrNames datasets)));
                    kernelModules = [ "zfs" "r8169" ];
                    availableKernelModules = config.boot.initrd.kernelModules;
                  };
                  zfs = {
                    requestEncryptionCredentials = true;
                    enableUnstable = true;
                    devNodes = "/dev/";
                  };
                };
                environment = {
                  persistence = let
                    dir = "/home/shadowrylander/aiern";
                    dirExists = pathExists dir;
                    repo = if dirExists then
                      (fetchGit {
                        url = "file://${dir}";
                        ref = "main";
                      })
                    else
                      inputs.${lib.bundle.attrs.users.primary};
                    reallyUnique = list:
                      let
                        attrs = remove null (map (item:
                          if (isAttr item) then
                            (item.file or item.directory)
                          else
                            null) list);
                      in unique (filter (item: !(elem item list)) list);
                    mergePersisted = set: list:
                      reallyUnique (map (item:
                        if (isString item) then
                          (recursiveUpdate { inherit item; } set)
                        else
                          (recursiveUpdate set item)) (flatten list));
                  in {
                    "/persist/root" = let
                      rootDirSet = {
                        user = "root";
                        group = "root";
                      };
                      rootFileSet.parentDirectory = rootDirSet;
                      etc-prefixes = [
                        "nixos"
                        "containers"
                        "NetworkManager/system-connections"
                        "tailscale"
                      ];
                    in {
                      hideMounts = true;
                      files = mergePersisted rootFileSet [
                        "/etc/host"
                        "/etc/machine-id"
                        (map (directory:
                          bundle.imports.list {
                            dir = "${repo}/${directory}";
                            root = true;
                            file.prefix.post = "/${directory}";
                            ignores = {
                              prefix = etc-prefixes;
                              dirs = true;
                            };
                            recursive = true;
                          }) [ "etc" "var" ])
                      ];
                      directories = mergePersisted rootDirSet [
                        (map (d: "/etc/" + d) etc-prefixes)

                        "/bin"

                        # TODO: Prevents `sshd_config' itself from being created
                        # "/etc/ssh"

                        "/sbin"
                        "/snap"
                        "/usr"
                        "/var/lib/acme"
                        "/var/lib/bluetooth"
                        "/var/lib/systemd/coredump"
                        "/var/log"

                        # Managed by the `var' module
                        # "/var/lib/tailscale"
                      ];
                    };
                    "/persist" = let
                      redRepoFiles = flatten [ (bundle.dirCon.others repo) ];
                      redRepoDirectories =
                        flatten [ (bundle.dirCon.dirs repo) ];
                    in {
                      users = mapAttrs' (designation: user:
                        let
                          home = bundle.attrs.allHomes.${designation};
                          pHome = "/persist/${home}";
                          userDirSet = {
                            inherit user;
                            group = user;
                          };
                          userFileSet.parentDirectory = userDirSet;
                          predRepoFiles =
                            flatten [ (bundle.dirCon.others pHome) ];
                          predRepoDirectories =
                            flatten [ (bundle.dirCon.dirs pHome) ];
                        in nameValuePair user {
                          inherit home;
                          files = mergePersisted userFileSet [
                            ".bash_history"
                            ".emacs-profile"
                            ".fasd"
                            ".gitignore"
                            ".globalignore"
                            ".nix-channels"
                            ".python-history"
                            ".screenrc"
                            ".viminfo"
                            ".zsh_history"
                            config.services.caddy.dataDir
                            redRepoFiles
                            predRepoFiles
                          ];
                          directories = mergePersisted userDirSet [
                            ".atom"
                            ".byobu"
                            ".cache"
                            ".config"
                            ".linuxbrew"
                            ".local"
                            ".mozilla"
                            ".peru"
                            ".pki"
                            ".repos"
                            ".user"
                            ".vim_runtime"
                            ".virtualenvs"
                            ".vscode-oss"
                            ".vscode"
                            ".yubico"
                            ".z"
                            "Documents"
                            "Downloads"
                            "keybase"
                            "Music"
                            "nix-plugins"
                            "Pictures"
                            "Public"
                            "Templates"
                            "tests"
                            "Videos"
                            "VirtualBox VMs"
                            {
                              directory = ".gnupg";
                              mode = "0700";
                            }
                            {
                              directory = ".nixops";
                              mode = "0700";
                            }
                            {
                              directory = ".ssh";
                              mode = "0700";
                            }
                            {
                              directory = ".gnupgk";
                              mode = "0700";
                            }
                            redRepoDirectories
                            predRepoDirectories
                          ];
                        }) bundle.attrs.allUsers;
                    };
                  };
                };
                fileSystems = with lib;
                  let
                    forceMountpoint = dataset: mountpoint:
                      mkForce (recursiveUpdate bundle.attrs.fileSystems.base {
                        device = dataset;
                        ${
                          bundle.mif.null ((bundle.has.infix [
                            bundle.attrs.users.primary
                            "persist"
                            "home"
                          ] dataset) || (elem dataset [ ])) "neededForBoot"
                        } = true;
                      });
                    regularDatasets = filterAttrs (n: v: !(isList v)) datasets;
                    filteredDatasets = filterAttrs (n: isList) datasets;
                    listedDatasets = listToAttrs (flatten
                      (mapAttrsToList (dataset: map (nameValuePair dataset))
                        filteredDatasets));
                  in mapAttrs' (dataset: mountpoint:
                    nameValuePair mountpoint
                    (forceMountpoint dataset mountpoint))
                  (recursiveUpdate regularDatasets listedDatasets);
                services = {
                  sanoid = let
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
                      interval = "30s";

                      # 6 snapshots an hour
                      daily = 144;

                      # 2 snapshots a minute
                      hourly = 120;

                      # 6 snapshots a day for 28 days
                      monthly = 168;

                      # Twice the weeks in a year
                      yearly = 104;
                    };

                    datasets = listToAttrs (map
                      (dataset: nameValuePair "${host}/${dataset}" sanoidBase)
                      (flatten [ bundle.attrs.datasets.backup host ]));
                  };
                  syncoid = let
                    syncoidBase = mkMerge [
                      {
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
                    sshKey = "/root/.ssh/id_ed25519";
                    commands = listToAttrs (map (dataset:
                      nameValuePair "${host}/${dataset}"
                      (recursiveUpdate syncoidBase { target = ""; }))
                      (flatten [ bundle.attrs.datasets.backup host ]));
                  };
                  udev.extraRules = ''
                    ACTION=="add|change", KERNEL=="sd[a-z]*[0-9]*|mmcblk[0-9]*p[0-9]*|nvme[0-9]*n[0-9]*p[0-9]*", ENV{ID_FS_TYPE}=="zfs_member", ATTR{../queue/scheduler}="none"
                  ''; # zfs already has its own scheduler. without this my(@Artturin) computer froze for a second when i nix build something.
                  zfs = {
                    trim.enable = true;
                    autoScrub.enable = true;

                    # Managed by Sanoid
                    autoSnapshot.enable = false;
                  };
                };

                virtualisation = {
                  containers.storage.settings.storage.driver = "zfs";
                  lxd.zfsSupport = true;
                  podman.extraPackages = [ pkgs.zfs ];
                  docker.storageDriver = "zfs";
                };

              });
            fonts = { config, pkgs, ... }: {
              console.font = "Cartograph CF Extra Bold Italic";
              keyMap = "us";
              fonts = pkgs.nerdfonts.override { fonts = [ "CascadiaCode" ]; };
            };
            memory = { config, ... }: {
              zramSwap = {
                enable = true;
                algorithm = "zstd";
              };
            };
            fileSystems = args@{ config, ... }: {
              fileSystems = let inherit (bundle.attrs.fileSystems) base;
              in if config.variables.zfs then
                (bundle.iron.getAttrs config.configs.hardware.fileSystems [
                  "/boot"
                  "/boot/efi"
                ])
              else
                config.configs.hardware.fileSystems;
            };
            hardware = args@{ config, ... }: {
              hardware = {
                inherit (config.configs.hardware.hardware) cpu;
                enableRedistributableFirmware = lib.mkDefault true;
                # Enable sound
                pulseaudio.enable = mkForce true;
              };
              sound.enable = true;
            };
            programs = { config, ... }: {
              environment.pathsToLink = flatten [
                (optional (!config.programs.bash.enable)
                  "/share/bash-completion")
                (optional (!config.programs.zsh.enable) "/share/zsh")
                (optional (!config.programs.fish.enable) "/share/fish")
              ];
              programs = {
                fuse.userAllowOther = true;
                mosh = {
                  enable = true;
                  openFirewall = true;
                };
              };
            };
            services = { config, ... }: {
              services = {
                tailscale.enable = true;
                xserver = {
                  enable = true;
                  layout = "us";
                  # xkbOptions = "eurosign:e";
                  # Enable touchpad support.
                  libinput = {
                    enable = true;
                    touchpad = {
                      naturalScrolling = true;
                      middleEmulation = true;
                      tapping = true;
                    };
                  };
                  # synaptics.enable = true;
                  desktopManager.gnome.enable = true;
                  displayManager = {
                    startx.enable = true;
                    lightdm.enable = mkForce false;
                  };
                  autorun = false;
                };
              };
            };
            security = { config, pkgs, ... }: {
              environment = {
                shellInit = ''
                  export GPG_TTY="$(tty)"
                  gpg-connect-agent /bye
                  export SSH_AUTH_SOCK="/run/user/$UID/gnupg/S.gpg-agent.ssh"
                  echo UPDATESTARTUPTTY | gpg-connect-agent
                '';
                systemPackages = with pkgs; [ pinentry-curses ];
              };
              programs = {
                # Some programs need SUID wrappers, can be configured further or are
                # started in user sessions.
                # mtr.enable = true;
                gnupg.agent = {
                  enable = true;
                  enableSSHSupport = true;
                  pinentryFlavor = "curses";
                };
                ssh.startAgent = !config.programs.gnupg.agent.enableSSHSupport;
              };
              security.pam.enableSSHAgentAuth = true;
            };
            yubikey = { config, pkgs, ... }:
              mkIf config.variables.client {
                # For Yubikey SSH-GPG Authentication
                environment = {
                  systemPackages = with pkgs; [
                    pcsctools
                    yubico-pam
                    yubico-piv-tool
                    yubikey-manager
                    yubikey-agent
                    yubikey-personalization
                    yubioath-flutter
                    yubikey-manager-qt
                    yubikey-personalization-gui
                  ];
                };
                security.pam = {
                  yubico = {
                    enable = true;
                    debug = true;
                    mode = "challenge-response";
                  };
                };
                services = {
                  udev.packages = with pkgs; [
                    libu2f-host
                    libyubikey
                    yubikey-personalization
                  ];
                  pcscd.enable = true;
                };
              };
            nix = { config, registry, pkgs, ... }: {
              nix = rec {
                inherit registry;
                package = pkgs.nixUnstable;
                gc = mkMerge [
                  { automatic = true; }
                  {
                    dates = "monthly";
                  }
                  # {
                  #   dates = "monthly";
                  #   options = "-d";
                  # }
                  # {
                  #   dates = "daily";
                  #   options = "--delete-older-than 30d";
                  # }
                ];
                optimise = {
                  automatic = true;
                  dates = [ "05:00" ];
                };
                extraOptions = bundle.attrs.configs.nix;
                settings = {
                  auto-optimise-store = true;
                  sandbox = true;
                };
                # sandboxPaths = [];
              };
            };
            networking = args@{ config, host, ... }: {
              networking = {
                hostName = host;
                networkmanager.enable = mkForce true;
                interfaces = mapAttrs (n:
                  flip recursiveUpdate {
                    useDHCP =
                      mkForce (!config.networking.networkmanager.enable);
                    wakeOnLan.enable = true;
                  }) (removeAttrs config.configs.config.networking.interfaces
                    [ "wg0" ]);

                # The global useDHCP flag is deprecated, therefore explicitly set to false here.
                # Per-interface useDHCP will be mandatory in the future, so this generated config
                # replicates the default behaviour.
                useDHCP = false;

                # Configure network proxy if necessary
                # proxy = {
                # default = "http://user:password@proxy:port/";
                # noProxy = "127.0.0.1,localhost,internal.domain";
                # };

                wireguard.interfaces.wg0 = {
                  generatePrivateKeyFile = true;
                  privateKeyFile = "/persist/etc/wireguard/wg0";
                };

                firewall = recursiveUpdate { enable = true; }
                  (if config.variables.relay then {
                    allowedTCPPorts = [ 80 222 443 2022 8080 9418 ];
                  } else if config.variables.server then {
                    allowedTCPPorts = [ ];
                  } else {
                    allowedTCPPorts = [ ];
                  });
              };
            };
            environment = { config, pkgs, ... }: {
              environment = {
                systemPackages = with pkgs;
                  flatten [
                    (pass.withExtensions
                      (ext: with ext; [ pass-tomb pass-genphrase ]))
                    (Python.withPackages
                      (ppkgs: with ppkgs; [ bakery nixpkgs ]))
                    assh
                    cached-nix-shell
                    cachix
                    coreutils
                    direnv
                    exa
                    fasd
                    fd
                    fzf
                    git
                    git-crypt
                    gnupg
                    gopass
                    gum
                    hub
                    mosh
                    sqlite
                    starship
                    tailscale
                    tailapi
                    uutils-coreutils
                    yadm
                    zoxide
                  ];
              };
            };
            users = { config, pkgs, ... }: {
              users = with bundle.attrs.users;
                let
                  base = {
                    hashedPassword =
                      "$6$DoC/h6kR66Sa$aZKtTOXAqnan/jAC.4dH9tCYshheiKUZItR4g/kmMMLsfLQh0KslINL9zUTX2IjAZh9DE18eAh1AAz48.n/cm.";
                    isNormalUser = true;
                    createHome = true;
                    extraGroups = [
                      "wheel"
                      "networkmanager"
                      "persist"
                      "libvirtd"
                      "docker"
                      "adbusers"
                    ];
                    openssh.authorizedKeys.keys =
                      unique (flatten [ (attrValues bundle.attrs.ssh.keys) ]);
                  };
                in rec {
                  users = mkMerge [
                    (genAttrs bundle.attrs.allUsernames (user: base))
                    {
                      "${primary}" = {
                        uid = 4362;
                        home = bundle.attrs.homes.primary;
                        description = "Jeet Ray";
                        group = primary;
                        extraGroups = [ secondary ];
                        # shell = pkgs.xonsh;
                        shell = pkgs.zsh;
                      };
                      "${secondary}" = {
                        uid = 1111;
                        home = bundle.attrs.homes.secondary;
                        description = "Alicia Summers";
                        group = secondary;
                        extraGroups = [ primary ];
                        shell = pkgs.fish;
                      };
                      "${nightingale}" = {
                        uid = 8888;
                        home = bundle.attrs.homes.nightingale;
                        description = "Curtis Nightingale";
                        group = "root";
                        extraGroups = [ primary secondary ];
                        shell = pkgs.zsh;
                      };
                      root = {
                        shell = mkForce pkgs.zsh;
                        home = bundle.attrs.allHomes.root;
                        isNormalUser = mkForce false;
                        isSystemUser = mkForce true;
                      };
                    }
                  ];

                  mutableUsers = false;

                  groups = {
                    "${primary}" = {
                      gid = config.users.users.${primary}.uid;
                      members = [ primary secondary nightingale ];
                    };
                    "${secondary}" = {
                      gid = config.users.users.${secondary}.uid;
                      members = [ primary secondary nightingale ];
                    };
                    "${nightingale}" = {
                      gid = config.users.users.${nightingale}.uid;
                      members = [ nightingale ];
                    };
                  };
                };
            };
          })
        ];
        nixosModules = bundle.fold.set [
          miniModules
          (rec {
            boot = { config, pkgs, ... }:
              with pkgs; {
                boot = {
                  binfmt.emulatedSystems = [ "armv7l-linux" "aarch64-linux" ];
                  extraModulePackages = with config.boot.kernelPackages;
                    [
                      # anbox
                      # wireguard
                    ];
                  kernelPatches = let
                    genCK = ignores:
                      map (name: {
                        inherit name;
                        patch = "${toString ck}/${name}";
                      }) (pipe flake.inputs.ck [
                        readDir
                        attrNames
                        (filter (p:
                          !(elem p (flatten [
                            "series"
                            "0016-Add-ck1-version.patch"
                            ignores
                          ]))))
                      ]);
                    versionhas.infix = infix:
                      bundle.has.infix (toList infix)
                      config.boot.kernelPackages.version;

                    # TODO: Doesn't work with the surface kernel; causes it to overheat during suspend
                  in unique (flatten [
                    # (bundle.mif.list (versionhas.infix [ "xanmod" "zen" "lqx"]) (genCK [
                    #     "0001-MultiQueue-Skiplist-Scheduler-v0.210.patch"
                    #     "0002-Unmask-ondemand-and-conservative-and-allow-schedutil.patch"
                    #     "0003-Make-preemptible-kernel-default.patch"
                    #     "0007-Replace-all-schedule-timeout-1-with-schedule_min_hrt.patch"
                    #     "0011-Make-hrtimer-granularity-and-minimum-hrtimeout-confi.patch"
                    #     "0012-Make-threaded-IRQs-optionally-the-default-which-can-.patch"
                    #     "0015-Make-nohz_full-not-be-picked-up-as-a-default-config-.patch"
                    # ]))
                    # (bundle.mif.list (versionhas.infix "xanmod") (genCK [
                    #     "0013-Reinstate-default-Hz-of-100-in-combination-with-MuQS.patch"
                    #     "0014-Swap-sucks.patch"
                    # ]))
                    # (genCK [
                    #     # "0004-Create-highres-timeout-variants-of-schedule_timeout-.patch"
                    #     # "0005-Special-case-calls-of-schedule_timeout-1-to-use-the-.patch"
                    #     # "0006-Convert-msleep-to-use-hrtimers-when-active.patch"
                    #     # "0008-Replace-all-calls-to-schedule_timeout_interruptible-.patch"
                    #     # "0009-Replace-all-calls-to-schedule_timeout_uninterruptibl.patch"
                    #     # "0010-Don-t-use-hrtimer-overlay-when-pm_freezing-since-som.patch"
                    # ])
                  ]);

                };
              };
            caddy = { config, ... }: {
              services.caddy = mkIf config.variables.relay {
                enable = true;
                ca = null;
                config =
                  readFile "/etc/caddy/files/${config.networking.hostName}";
                adapter = "yaml";
              };
            };
            environment = { config, pkgs, ... }:
              with pkgs; {
                environment = {
                  systemPackages = [
                    # ddar
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
                    google-chrome
                    google-chrome-beta
                    google-chrome-dev
                    gotop
                    gparted
                    gptfdisk
                    inetutils
                    jupyter
                    keybase-gui
                    kitty
                    libffi
                    libguestfs
                    libsForQt5.qtstyleplugin-kvantum
                    libtool
                    lolcat
                    lorri
                    man
                    meld
                    melt
                    micro
                    mkpasswd
                    monkeysphere
                    mtr
                    neo-cowsay
                    neovim
                    nickel
                    niv
                    nnn
                    nodePackages.prettier
                    nox
                    ntfs3g
                    nixos-shell
                    pandoc
                    par2cmdline
                    parted
                    pmutils
                    peru
                    pfetch
                    pypy
                    Python
                    ranger
                    refind
                    ripgrep
                    ripgrep-all
                    rsync
                    sd
                    bundle
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
                    vivaldi
                    vivaldi-ffmpeg-codecs
                    vivaldi-widevine
                    vlc
                    vscode
                    vscodium
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
                  ]) ++ (with pkgs.gnome; [ dconf-editor ])
                    ++ (with pkgs.gitAndTools; [
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
              };
            fonts = { config, pkgs, ... }: {
              fonts.fonts = with pkgs;
                flatten [
                  (pkgs.cartograph-cf-all or [ ])
                  (nerdfonts.override { fonts = [ "Agave" "CascadiaCode" ]; })
                ];
            };
            miscellaneous = { config, pkgs, system, ... }: {
              xdg.portal = {
                enable = true;
                extraPortals =
                  map (portal: pkgs."xdg-desktop-portal-${portal}") [
                    "gtk"
                    "kde"
                  ];
              };
              i18n = {
                # Select internationalisation properties.
                defaultLocale = "en_US.UTF-8";
              };
              time.timeZone = "America/Toronto";
              system = {
                inherit (nixos-configurations.configuration.config.system)
                  stateVersion;
                autoUpgrade = {
                  enable = true;
                  allowReboot = false;
                  flake =
                    "https://github.com/nixos/nixpkgs/archive/${inputs.nixpkgs.rev}.tar.gz";
                };
              };
            };
            nix-direnv = { config, pkgs, ... }: {
              environment = {
                pathsToLink = [ "/share/nix-direnv" ];
                systemPackages = [ pkgs.nix-direnv ];
              };
            };
            programs = { config, ... }: {
              programs = { extra-container.enable = true; };
            };
            tailscale = { config, ... }: {
              services.tailscale = {
                apifile = "/etc/tailscale/apikeys/jeet.ray";
                autoconnet = true;
                deleteHostBeforeAuth = true;
                # exitNode = mkIf (! config.services.tailscale.exitNode.advertise) {
                #     device = "bastiodon";
                #     allowLANAccess = true;
                # };
                magicDns = {
                  enable = true;
                  searchDomains = toList "alpaca-python.ts.net";
                  nameservers = [
                    # Cloudflare
                    # "1.1.1.1"
                    # "1.0.0.1"

                    # Google
                    # "8.8.8.8"
                    # "8.8.4.4"

                    # Quad9
                    # "9.9.9.9"
                    # "149.112.112.112"

                    # Adguard
                    "94.140.14.14"
                    "94.140.15.15"
                  ];
                };
                openFirewall = true;
                state.file =
                  "/etc/tailscale/states/${config.networking.hostName}/tailscaled.state";
                trustInterface = true;
              };
            };
            services = { config, pkgs, system, host, ... }: {
              services = {
                emacs = {
                  package = pkgs.emacsGcc;
                  enable = true;
                  defaultEditor = true;
                };
                flatpak.enable = true;
                guix.enable = true;
                printing.enable = true;
              };
            };
            systemd = { config, pkgs, host, ... }: {
              systemd = let
                replace = replaceStrings [ "borgmatic-" ] [ "" ];
                replace = replaceStrings [ "rclone-" ] [ "" ];
                bundle.systemd.wants = list:
                  unique ([ "network.target" "network-online.target" ] ++ list);
              in {
                # packages = with pkgs; [ runit ];
                services = let
                  mkBaseWants = list:
                    bundle.systemd.wants
                    ([ "rclone-backblazeB2.service" ] ++ list);
                  mkBase = n: rec {
                    description = "Borgmatic ${(bundle.toCapital n)} Backup";
                    unitConfig.ConditionACPower = "true";
                    serviceConfig = {
                      CPUSchedulingPolicy = "batch";
                      ExecStart = ''
                        ${pkgs.systemd}/bin/systemd-inhibit --who=\"${description}\" \
                                                            --why=\"Prevent interrupting scheduled backup for `${description}'\" \
                                                            ${pkgs.borgmatic}/bin/borgmatic \
                                                            --verbosity -1 \
                                                            --syslog-verbosity 1 \
                                                            --config /etc/borgmatic/${n}.yaml
                      '';
                      ExecStartPre = "${pkgs.coreutils}/bin/sleep 10m";
                      IOSchedulingClass = "best-effort";
                      IOSchedulingPriority = "7";
                      IOWeight = "100";
                      LockPersonality = "true";
                      LogRateLimitIntervalSec = "0";
                      MemoryDenyWriteExecute = "no";
                      Nice = "19";
                      NoNewPrivileges = "yes";
                      PrivateDevices = "yes";
                      PrivateTmp = "yes";
                      ProtectClock = "yes";
                      ProtectControlGroups = "yes";
                      ProtectHome = "tmpfs";
                      ProtectHostname = "yes";
                      ProtectKernelLogs = "yes";
                      ProtectKernelModules = "yes";
                      ProtectKernelTunables = "yes";
                      ProtectSystem = "strict";
                      Restart = "no";
                      RestrictAddressFamilies =
                        "AF_UNIX AF_INET AF_INET6 AF_NETLINK";
                      RestrictNamespaces = "yes";
                      RestrictRealtime = "yes";
                      RestrictSUIDSGID = "yes";
                      SystemCallArchitectures = "native";
                      SystemCallErrorNumber = "EPERM";
                      SystemCallFilter = "@system-service";
                      Type = "oneshot";
                    };
                  };
                  mkBase = n:
                    let mountdir = "/mnts/rclone/${n}";
                    in rec {
                      description = "Rclone ${bundle.toCapital n} Mount";
                      wants = bundle.systemd.wants [ ];
                      after = wants;
                      serviceConfig = {
                        ExecStartPre =
                          "/run/current-system/sw/bin/mkdir -p ${mountdir}";
                        ExecStop =
                          "/run/wrappers/bin/fusermount -u ${mountdir}";
                        RestartSec = "10s";
                        Type = "notify";
                      };
                    };

                  mkBaseExecStart = mount:
                    let
                      dir-cache-time = "96h";
                      buffer-size = "512M";
                    in ''
                      ${pkgs.rclone}/bin/rclone mount \
                                                ${mount} \
                                                /mnts/rclone/${
                                                  head (splitString ":" mount)
                                                } \
                                                --config ${bundle.attrs.homes.primary}/rclone.conf \
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
                  (mapAttrs (n:
                    recursiveUpdate
                    (recursiveUpdate bundle.attrs.configs.services.base
                      (mkBase (replace n)))) {
                        borgmatic-oreo = rec {
                          wants =
                            mkBaseWants [ "chimchar-oreo.mount" "oreo.mount" ];
                          after = wants;
                          serviceConfig = {
                            ReadWritePaths = "-/oreo";
                            ReadOnlyPaths = "-/chimchar/oreo";
                          };
                        };
                        borgmatic-oreo-rsync = rec {
                          wants = mkBaseWants [ "chimchar-oreo.mount" ];
                          after = wants;
                          serviceConfig.ReadOnlyPaths = "-/chimchar/oreo";
                        };
                        borgmatic-chimchar = rec {
                          wants =
                            mkBaseWants [ "chimchar.mount" "infernape.mount" ];
                          after = wants;
                          serviceConfig = {
                            ReadWritePaths = "-/infernape";
                            ReadOnlyPaths = "-/chimchar";
                          };
                        };
                        borgmatic-user = rec {
                          wants = mkBaseWants [
                            "${
                              replaceStrings [ "/" ] [ "-" ] (removeSuffix "/"
                                (removePrefix "/" bundle.attrs.homes.primary))
                            }.mount"
                          ];
                          after = wants;
                          serviceConfig = {
                            BindPaths =
                              "-/home/shadowrylander/aiern -${bundle.attrs.homes.primary}/.user";
                          };
                        };
                      })
                  (mapAttrs (n:
                    recursiveUpdate
                    (recursiveUpdate bundle.attrs.configs.services.base
                      (mkBase (replace n)))) {
                        rclone-backblazeB2.serviceConfig.ExecStart =
                          mkBaseExecStart
                          "backblazeB2:borgbackups-53f2bd74-148c-4fe7-bdb5-701d325645a6";
                      })
                  {
                    # wstunnel-http = mkIf config.variables.relay (recursiveUpdate (bundle.attrs.configs.services.mkBase "root") {
                    #     serviceConfig = {
                    #         ExecStart = "${pkgs.wstunnel}/bin/wstunnel --server ws://0.0.0.0:80 -r 127.0.0.1:880";
                    #     };
                    # });
                    # iodine = mkIf config.variables.relay (recursiveUpdate (bundle.attrs.configs.services.mkBase "root") {

                    # });
                    # wstunnel-tls = mkIf config.variables.relay (recursiveUpdate (bundle.attrs.configs.services.mkBase "root") {
                    wstunnel = mkIf config.variables.relay (recursiveUpdate
                      (bundle.attrs.configs.services.mkBase "root") {
                        serviceConfig = {
                          ExecStart =
                            "${pkgs.wstunnel}/bin/wstunnel wss://0.0.0.0:443 -r 127.0.0.1:32443";
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
                in mapAttrs (n: recursiveUpdate (mkBase (replace n))) {
                  borgmatic-oreo = { };
                  borgmatic-oreo-rsync = { };
                  borgmatic-chimchar = { };
                  borgmatic-user = {
                    timeConfig = {
                      OnCalender = "*-*-* */3:00:00";
                      RandomizedDelaySec = "90min";
                    };
                  };
                };
              };
            };

            virtualisation = { config, pkgs, ... }: {
              virtualisation = {
                containers.storage.settings.storage = {
                  graphroot = "/var/lib/containers/storage";
                  runroot = "/run/containers/storage";
                };
                xen.enable = false;
                podman = {
                  enable = true;
                  dockerCompat = true;
                };
                oci-containers.backend = "podman";
                docker = {
                  enable = true;
                  package = pkgs.docker;
                  enableOnBoot = true;
                };
                libvirtd.enable = true;
              };
            };

            minimal = { config, ... }: { imports = attrValues miniModules; };
            default = minimal;
          })
        ];
        nixosModule = nixosModules.default;
        defaultNixosModule = nixosModule;
      };
      callPackages = { };
      overlays = let
        calledPackages = mapAttrs (pname: v: final: prev: {
          "${pname}" = final.callPackage v { inherit pname; };
        }) (filterAttrs (n: isFunction) callPackages);
        default = final: prev { inherit (calledPackages) strapper; };
      in bundle.fold.set [
        inputs.sylveon.overlays
        calledPackages
        {
          inherit default;
          lib = final: prev: { inherit lib; };
          emacs = final: prev: { emacs = final.emacsNativeComp; };
          xonsh = final: prev: {
            xonsh = bundle.mk.mkWithPackages prev.xonsh [
              "xontrib-sh"
              "xontrib-readable-traceback"
              "xontrib-pipeliner"
              "xonsh-autoxsh"
              "xonsh-direnv"
            ] "bakery";
          };
        }
      ];
      make = system: overlays:
        let made = bundle.make system overlays;
        in bundle.fold.set [
          made
          (rec {
            specialArgs = name:
              bundle.fold.set [
                made.specialArgs
                self
                self.${system}
                {
                  host = name;
                  inherit inputs;
                }
              ];
            configs = rec {
              base = n: v: {
                inherit system;
                specialArgs = specialArgs n;
              };
              default = n: v:
                nixosSystem (bundle.fold.set [
                  (base n v)
                  {
                    modules = flatten [
                      v
                      (map (path:
                        optional (pathExists path) (getFlake path).nixosModules)
                        [ ./secrets ])
                      (map (dir:
                        optional (pathExists dir) (bundle.list {
                          inherit dir;
                          ignores.elem = [ "flake.nix" ];
                        })) [ ./secrets ])
                      (map (m: attrValues m.nixosModules) [
                        inputs.home-manager-config
                        nixosModules
                      ])
                    ];
                  }
                ]);
              mini = n: v:
                nixosSystem (bundle.fold.set [
                  (base n v)
                  { modules = flatten [ v nixosModules.minimal ]; }
                ]);
            };
          })
        ];
    in mkOutputs {
      inherit inputs self;
      inherit (inputs.strapper) pname isApp type;
      callPackage = { bakery, pname }:
        Python.pkgs.buildPythonPackage rec {
          inherit pname;
          version = "1.0.0.0";
          src = ../../strapper;
          propagatedBuildInputs = [ bakery ];
        };
      inherit overlays make;
      extraSystemOutputs = oo: system: rec {
        nixosConfigurations = bundle.fold.set [
          (mapAttrs made.configs.default hosts)
          (mapAttrs made.configs.mini
            (bundle.mapAttrNames (n: v: "mini-" + n) hosts))
        ];
        packages = filterPackages system (bundle.fold.set [
          (made.mkPackages.default nixpkgs)
          made.mkPackages.python
        ]);
        devShells = with pkgs; rec {
          git-shell = mkShell {
            buildInputs = [ git-crypt git-filter-repo rsync realpath ];
          };
        };
      };
      extraOutputs =
        bundle.fold.set [ nixosModules { inherit make lib hosts; } ];
    };
}
