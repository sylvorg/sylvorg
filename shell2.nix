with builtins; let
    inherit ((import ./etc/nixos).make.specialArgs "nixos" currentSystem) lib pkgs;
    datasets = import ./etc/nixos/datasets.nix "nixos";
    fileSystems = with lib; let
        forceMountpoint = dataset: mountpoint: mkForce (recursiveUpdate base { device = dataset; ${
            j.mif.null ((j.has.infix [
                j.attrs.users.primary
                "persist"
                "home"
            ] dataset) || (elem dataset [ ])) "neededForBoot"
        } = true; });
        regularDatasets = filterAttrs (n: v: ! isList v) datasets;
        filteredDatasets = filterAttrs (n: v: isList v) datasets;
        listedDatasets = listToAttrs (flatten (mapAttrsToList (dataset: mountlist: map (mountpoint: nameValuePair dataset mountpoint) mountlist) filteredDatasets));
    in mapAttrs' (dataset: mountpoint: nameValuePair mountpoint (forceMountpoint dataset mountpoint)) (recursiveUpdate regularDatasets listedDatasets);
in pkgs.mkShell {
    shellHook = ''${trace (lib.flatten (attrValues fileSystems)) "echo"}; exit'';
}