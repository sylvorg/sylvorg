{ config, lib, hostName, ... }: with builtins; with lib; with j; {
    networking = let
        primus = "58a4bafc38c2cc6e91ba27c7f1ca910c8fba857ee02f03648450b963db2519fc";
    in {
        inherit hostName;
        wireless = {
            # enable = true; # Enables wireless support via wpa_supplicant.
            enable = false; # Enables wireless support via wpa_supplicant.
            networks = {
                "Primus-17FE-2.4" = {
                    pskRaw = primus;
                    priority = 0;
                };
                "Primus-17FE" = {
                    pskRaw = primus;
                    priority = 1;
                };
            };
        };
        networkmanager.enable = mkForce true;

        # The global useDHCP flag is deprecated, therefore explicitly set to false here.
        # Per-interface useDHCP will be mandatory in the future, so this generated config
        # replicates the default behaviour.
        useDHCP = false;

        # Configure network proxy if necessary
        # proxy = {
        # default = "http://user:password@proxy:port/";
        # noProxy = "127.0.0.1,localhost,internal.domain";
        # };

        # Open ports in the firewall.
        # firewall = {
        # allowedTCPPorts = [ ... ];
        # allowedUDPPorts = [ ... ];

        # Or disable the firewall altogether.
        # enable = false;
        # };

        # wireguard.interfaces.wg0 = {
        #   generatePrivateKeyFile = true;
        #   privateKeyFile = "/persist/etc/wireguard/wg0";
        # };
    };
}
