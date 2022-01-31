{ config, ... }:

{
    boot = {
        supportedFilesystems = [ "bcachefs" ];
        initrd.supportedFilesystems = config.boot.loader.supportedFilesystems;
    };
}
