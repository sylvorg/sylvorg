# Management of static files in /var.

{ config, lib, pkgs, ... }:

with lib;

let

  flake = import ..;

  var' = filter (f: f.enable) (attrValues config.environment.vars);

  var = pkgs.runCommandLocal "var" {
    # This is needed for the systemd module
    passthru.targets = map (x: x.target) var';
  } /* sh */ ''
    set -euo pipefail

    makevarEntry() {
      src="$1"
      target="$2"
      mode="$3"
      user="$4"
      group="$5"

      if [[ "$src" = *'*'* ]]; then
        # If the source name contains '*', perform globbing.
        mkdir -p "$out/var/$target"
        for fn in $src; do
            ln -s "$fn" "$out/var/$target/"
        done
      else

        mkdir -p "$out/var/$(dirname "$target")"
        if ! [ -e "$out/var/$target" ]; then
          ln -s "$src" "$out/var/$target"
        else
          echo "duplicate entry $target -> $src"
          if [ "$(readlink "$out/var/$target")" != "$src" ]; then
            echo "mismatched duplicate entry $(readlink "$out/var/$target") <-> $src"
            ret=1

            continue
          fi
        fi

        if [ "$mode" != symlink ]; then
          echo "$mode" > "$out/var/$target.mode"
          echo "$user" > "$out/var/$target.uid"
          echo "$group" > "$out/var/$target.gid"
        fi
      fi
    }

    mkdir -p "$out/var"
    ${concatMapStringsSep "\n" (varEntry: escapeShellArgs [
      "makevarEntry"
      # Force local source paths to be added to the store
      "${varEntry.source}"
      varEntry.target
      varEntry.mode
      varEntry.user
      varEntry.group
    ]) var'}
  '';

in

{

  imports = [ "${flake.inputs.nixpkgs}/nixos/modules/system/build.nix" ];

  ###### interface

  options = {

    environment.vars = mkOption {
      default = {};
      example = literalExpression ''
        { example-configuration-file =
            { source = "/nix/store/.../var/dir/file.conf.example";
              mode = "0440";
            };
          "default/useradd".text = "GROUP=100 ...";
        }
      '';
      description = ''
        Set of files that have to be linked in <filename>/var</filename>.
      '';

      type = with types; attrsOf (submodule (
        { name, config, options, ... }:
        { options = {

            enable = mkOption {
              type = types.bool;
              default = true;
              description = ''
                Whether this /var file should be generated.  This
                option allows specific /var files to be disabled.
              '';
            };

            target = mkOption {
              type = types.str;
              description = ''
                Name of symlink (relative to
                <filename>/var</filename>).  Defaults to the attribute
                name.
              '';
            };

            text = mkOption {
              default = null;
              type = types.nullOr types.lines;
              description = "Text of the file.";
            };

            source = mkOption {
              type = types.path;
              description = "Path of the source file.";
            };

            mode = mkOption {
              type = types.str;
              default = "symlink";
              example = "0600";
              description = ''
                If set to something else than <literal>symlink</literal>,
                the file is copied instead of symlinked, with the given
                file mode.
              '';
            };

            uid = mkOption {
              default = 0;
              type = types.int;
              description = ''
                UID of created file. Only takes effect when the file is
                copied (that is, the mode is not 'symlink').
                '';
            };

            gid = mkOption {
              default = 0;
              type = types.int;
              description = ''
                GID of created file. Only takes effect when the file is
                copied (that is, the mode is not 'symlink').
              '';
            };

            user = mkOption {
              default = "+${toString config.uid}";
              type = types.str;
              description = ''
                User name of created file.
                Only takes effect when the file is copied (that is, the mode is not 'symlink').
                Changing this option takes precedence over <literal>uid</literal>.
              '';
            };

            group = mkOption {
              default = "+${toString config.gid}";
              type = types.str;
              description = ''
                Group name of created file.
                Only takes effect when the file is copied (that is, the mode is not 'symlink').
                Changing this option takes precedence over <literal>gid</literal>.
              '';
            };

          };

          config = {
            target = mkDefault name;
            source = mkIf (config.text != null) (
              let name' = "var-" + baseNameOf name;
              in mkDerivedConfig options.text (pkgs.writeText name')
            );
          };

        }));

    };

  };


  ###### implementation

  config = {

    system.build.var = var;
    system.build.varActivationCommands =
      ''
        # Set up the statically computed bits of /var.
        echo "setting up /var..."
        ${pkgs.perl.withPackages (p: [ p.FileSlurp ])}/bin/perl ${./setup-var.pl} ${var}/var
      '';
  };

}
