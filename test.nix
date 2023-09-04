with import <nixos> { };
with lib;
let
  magic = (attrValues (mapAttrs (protocol: port:
    mapAttrs' (n: v:
      nameValuePair "argus.wstunnel.${protocol}.${n}" {
        proxycommand = v + ''${toString port}"'';
      }) {
        magic = ''
          sh -c "wstunnel -L stdio:%h:%p wss://$(echo %h | cut -d '.' -f 1):'';
      }) {
        tls = 443;
        http = 80;
      }));
in magic
