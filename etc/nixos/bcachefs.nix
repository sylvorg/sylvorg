{ config, ... }:

{
    boot = {
        supportedFilesystems = [ "bcachefs" ];
        initrd.supportedFilesystems = config.boot.supportedFilesystems;
    };
}
