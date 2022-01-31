{ config, ... }:

{
    boot.loader = {
        supportedFilesystems = [ "bcachefs" ];
        initrd.supportedFilesystems = config.boot.loader.supportedFilesystems;
    };
}
