{ config, lib, pkgs, system, ... }: with lib;

{
    services = {
emacs = {
    package = pkgs.emacsGcc;
    enable = true;
    defaultEditor = true;
};
# flatpak.enable = j.attrs.no-arms;
flatpak.enable = true;
guix.enable = true;
printing.enable = true;
udev.packages = with pkgs; [
    yubikey-personalization
    libu2f-host
];
pcscd.enable = true;
zfs = mkIf j.attrs.zfs {
    trim.enable = true;
    autoScrub.enable = true;

    # Managed by Sanoid
    autoSnapshot.enable = false;
};
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

        # 6 snapshots an hour
        daily = 144;

        # 2 snapshots a minute
        hourly = 120;

        # 6 snapshots a day for 28 days
        monthly = 168;

        # Twice the weeks in a year
        yearly = 104;
    };

    datasets = listToAttrs (map (dataset: nameValuePair "${host}/${dataset}" sanoidBase) (flatten [
        j.attrs.datasets.backup
        host
    ]));
};
syncoid = let
    syncoidBase = mkMerge [{
        recursive = true;
        commonArgs = [
            "--compress zstd-slow"
            "--no-stream"
            "--no-sync-snap"
            "--create-bookmark"
        ];
        }
        # (mkIf vars.encrypted {
        #     sendOptions = "vvwRI";
        #     recvOptions = "vvFs";
        # })
        # (mkIf (!vars.encrypted) {
        #     recvOptions = "vvFds";
        #     sendOptions = "vvRI";
        # })
    ];
in {
    enable = false;
    sshKey = "/root/.ssh/id_ecdsa";
    commands = listToAttrs (map (dataset: nameValuePair "${host}/${dataset}" (syncoidBase // { target = ""; })) (flatten [
        j.attrs.datasets.backup
        host
    ]));
};
};}
