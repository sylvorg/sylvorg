{ config, lib, ... }:

{
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
        searchDomains = [
            # "shadowrylander.github.beta.tailscale.net"
            "sylvorg.github.beta.tailscale.net"
        ];
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
    state.file = "/etc/tailscale/states/${config.networking.hostName}/tailscaled.state";
    trustInterface = true;
  };
}
