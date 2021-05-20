{ config, lib, host, ... }: with builtins; with lib; with j; {
    boot = {
        kernelModules = [ "zfs" ];
        # loader.grub.zfsSupport = true;
        initrd = {
            postDeviceCommands = myIf.empty config.vars.zfs (mkAfter ''
                zfs rollback -r ${host}/system/root@blank
                zfs rollback -r ${host}/system/home@blank
            '');
            kernelModules = [ "zfs" ];
            availableKernelModules = [ "zfs" ];
        };
        zfs = {
            requestEncryptionCredentials = true;
            enableUnstable = true;
        };
        extraModulePackages = with config.boot.kernelPackages; [ zfsUnstable ];
    };
    # networking.hostId = substring 0 8 (readFile "/etc/machine-id");
}
