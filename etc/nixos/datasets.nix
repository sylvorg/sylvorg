host: {
  "${host}/system/root" = "/";
  "${host}/hold" = "/hold";
  "${host}/shadowrylander" = "/shadowrylander";
  "${host}/shadowrylander/oreo" =
    [ "/home/shadowrylander/oreo" "/home/frost/oreo" "/home/curtis/oreo" ];
  "${host}/shadowrylander/sylveon" = [
    "/home/shadowrylander/sylveon"
    "/home/frost/sylveon"
    "/home/curtis/sylveon"
  ];
  "${host}/shadowrylander/sylvorg" = [
    "/home/shadowrylander/sylvorg"
    "/home/frost/sylvorg"
    "/home/curtis/sylvorg"
  ];
  "${host}/shadowrylander/syvlorg" = [
    "/home/shadowrylander/syvlorg"
    "/home/frost/syvlorg"
    "/home/curtis/syvlorg"
  ];
  "${host}/shadowrylander/aiern" =
    [ "/home/shadowrylander/aiern" "/home/frost/aiern" "/home/curtis/aiern" ];
  "${host}/shadowrylander/uru" =
    [ "/home/shadowrylander/uru" "/home/frost/uru" "/home/curtis/uru" ];
  "${host}/system/home" = "/home";
  "${host}/system/home/root" = "/root";
  "${host}/system/home/shadowrylander" = "/home/shadowrylander";
  "${host}/system/home/frost" = "/home/frost";
  "${host}/system/home/curtis" = "/home/curtis";
  "${host}/system/nix" = "/nix";
  "${host}/system/persist" = "/persist";
  "${host}/system/persist/root" = "/persist/root";
  "${host}/system/persist/shadowrylander" = "/persist/shadowrylander";
  "${host}/system/persist/frost" = "/persist/frost";
  "${host}/system/persist/curtis" = "/persist/curtis";
  "${host}/system/tmp" = "/tmp";
  "${host}/system/tmp/nix" = "/tmp/nix";
  "${host}/virt" = "/var/lib";
  "${host}/virt/docker" = "/var/lib/docker";
  "${host}/virt/kvm" = "/var/lib/kvm";
  "${host}/virt/podman" = "/var/lib/podman";
  "${host}/virt/podman/shadowrylander" = "/var/lib/podman/shadowrylander";
  "${host}/virt/podman/frost" = "/var/lib/podman/frost";
  "${host}/virt/podman/curtis" = "/var/lib/podman/curtis";
  "${host}/virt/qemu" = "/var/lib/qemu";
  "${host}/virt/vagrant" = "/var/lib/vagrant";
  "${host}/virt/xen" = "/var/lib/xen";
  "${host}/sandshrew" = "/sandshrew";
  "${host}/sandshrew/jails" = "/sandshrew/jails";
  "${host}/sandshrew/jails/base" = "/sandshrew/jails/base";
}
