{
  description = "shadowrylander's home-manager config!";
  inputs = {
    bundle = {
      url = "file:///home/shadowrylander/shadowrylander/bundle";
      type = "git";
      submodules = true;
    };
    valiant.follows = "bundle/valiant";
    nixpkgs.follows = "bundle/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };

    bash-completion = {
      url = "github:scop/bash-completion";
      flake = false;
    };
    grml = {
      url = "github:grml/grml-etc-core";
      flake = false;
    };
    powerline = {
      url = "github:powerline/powerline";
      flake = false;
    };
    oh-my-tmux = {
      url = "github:gpakosz/.tmux";
      flake = false;
    };
    aleclearmind = {
      url = "github:aleclearmind/nested-tmux";
      flake = false;
    };
    nix-env-fish = {
      url = "github:lilyball/nix-env.fish";
      flake = false;
    };
    kitty-themes = {
      url = "github:dexpota/kitty-themes";
      flake = false;
    };

    user = {
      url = "git+file:///home/shadowrylander/shadowrylander";
      inputs.home-manager-config.follows = "";
      type = "git";
      submodules = true;
    };
  };
  outputs = inputs@{ self, flake-utils, ... }:
    with builtins;
    with inputs.bundle.lib;
    with flake-utils.lib;
    let
      sconcat = a: b: a + b;
      sepcat = a: b: a + " " + b;
      slashcat = a: b: a + "/" + b;
      dotcat = a: b: a + "." + b;
      newcat = concatStringsSep "\\n";
      convert = v:
        if (v == null) then
          "None"
        else if (isList v) then
          "[ ${concatStringsSep ", " (map convert v)} ]"
        else if (isInt v) then
          (toString v)
        else if (isBool v) then
          ''"${bundle.toCapital (boolToString v)}"''
        else if (isFunction v) then
          (v null)
        else
          v;
      convertAttrs = append: n: v:
        let eq = if append then "+=" else "=";
        in if (isAttrs v) then
          (mapAttrsToList
            (N: V: ''\$''${toUpper n}["${N}"] ${eq} ${convert V}'') v)
        else
          "\$${toUpper n} ${eq} ${convert v}";
      convertAssignment = n: v: "${n} = ${convert v}";
      SourceText = { config, name, ... }:
        with types; {
          options = {
            enable = mkSubmoduleEnableOption name;
            text = mkOption {
              type = nullOr lines;
              description = "";
              default = null;
            };
            source = mkOption {
              type = path;
              description = "";
            };
          };
          config.source =
            mkIf (config.text != null) (pkgs.writeText name config.text);
        };
      mkSubmoduleEnableOption = name:
        mkOption {
          default = true;
          example = false;
          description = if name ? _type && name._type == "mdDoc" then
            lib.mdDoc "Whether to enable ${name.text}."
          else
            "Whether to enable ${name}.";
          type = lib.types.bool;
        };
    in {
      homeModules = {
        assh = { config, options, ... }:
          let cfg = config.programs.assh;
          in {
            options.programs.assh = {
              defaults = mkOption {
                type = types.attrs;
                default = { };
                decription = "";
              };
              enable = mkEnableOption "assh";
              package = mkPackageOption pkgs "assh" { };
              alias = mkOption {
                type = with types; nullOr nonEmptyStr;
                default = null;
                description = "";
              };
              enableGlobalIntegration = mkOption {
                type = types.bool;
                default = true;
                description = "";
              };
              enableProfileIntegration = mkOption {
                type = types.bool;
                default = true;
                description = "";
              };
              enableBashIntegration = mkOption {
                type = types.bool;
                default = true;
                description = "";
              };
              enableZshIntegration = mkOption {
                type = types.bool;
                default = true;
                description = "";
              };
              enableFishIntegration = mkOption {
                type = types.bool;
                default = true;
                description = "";
              };
              enableElvishIntegration = mkOption {
                type = types.bool;
                default = true;
                description = "";
              };
              enableIonIntegration = mkOption {
                type = types.bool;
                default = true;
                description = "";
              };
              enableXonshIntegration = mkOption {
                type = types.bool;
                default = true;
                description = "";
              };
              hosts = mkOption {
                type = types.attrs;
                default = { };
                decription = "";
              };
              templates = mkOption {
                type = types.attrs;
                default = { };
                decription = "";
              };
              includes = mkOption {
                type = with types; listOf path;
                default = [ ];
                description = "";
              };
            };
            config = mkIf cfg.enable (mkMerge (flatten [
              {
                programs.assh = mkBefore {
                  alias = mkIf (cfg.alias == null)
                    "${cfg.package}/bin/assh wrapper ssh --";
                };
              }
              (genAttrs (p: {
                programs.${p} =
                  mkIf cfg."enable${bundle.toCapital p}Integration" {
                    ssh = cfg.alias;
                  };
              }) [ "bash" "zsh" "fish" "elvish" "ion" ])
              {
                home.shellAliases =
                  mkIf cfg.enableProfileIntegration { ssh = cfg.alias; };
                programs.xonsh = mkIf cfg.enableXonshIntegration {
                  ssh = f: "lambda args, stdin=None: $(${cfg.alias} @(args))";
                };
              }
              {
                home.file.".ssh/assh.yml".text = with cfg;
                  toJSON {
                    inherit includes templates hosts defaults;
                    ASSHBinaryPath = package;
                  };
              }
            ]));
          };
        borgmatic = { config, options, ... }:
          let
            cfg = config.programs.borgmatic;

            mkNullableOption = args:
              mkOption (args // {
                type = types.nullOr args.type;
                default = null;
              });

            mkRetentionOption = frequency:
              mkNullableOption {
                type = types.int;
                description =
                  "Number of ${frequency} archives to keep. Use -1 for no limit.";
                example = 3;
              };

            extraConfigOption = mkOption {
              type = with types;
                attrsOf (oneOf [ str bool path int (listOf str) ]);
              default = { };
              description = "Extra settings.";
            };

            consistencyCheckModule = with types;
              submodule {
                options = {
                  name = mkOption {
                    type = enum [ "repository" "archives" "data" "extract" ];
                    description = "Name of consistency check to run.";
                    example = "repository";
                  };
                  frequency = mkNullableOption {
                    type = strMatching "([[:digit:]]+ .*)|always";
                    description = "Frequency of this type of check";
                    example = "2 weeks";
                  };
                };
              };

            configModule = types.submodule {
              options = {
                location = {
                  sourceDirectories = mkOption {
                    type = types.listOf types.str;
                    description = "Directories to backup.";
                    example = literalExpression "[config.home.homeDirectory]";
                  };
                  repositories = mkOption {
                    type = types.listOf types.str;
                    description = "Paths to repositories.";
                    example = literalExpression
                      ''["ssh://myuser@myrepo.myserver.com/./repo"]'';
                  };
                  extraConfig = extraConfigOption;
                };

                storage = {
                  encryptionPasscommand = mkNullableOption {
                    type = types.str;
                    description =
                      "Command writing the passphrase to standard output.";
                    example = literalExpression
                      ''"''${pkgs.password-store}/bin/pass borg-repo"'';
                  };
                  extraConfig = extraConfigOption;
                };

                retention = {
                  keepWithin = mkNullableOption {
                    type = types.strMatching "[[:digit:]]+[Hdwmy]";
                    description =
                      "Keep all archives within this time interval.";
                    example = "2d";
                  };

                  keepSecondly = mkRetentionOption "secondly";
                  keepMinutely = mkRetentionOption "minutely";
                  keepHourly = mkRetentionOption "hourly";
                  keepDaily = mkRetentionOption "daily";
                  keepWeekly = mkRetentionOption "weekly";
                  keepMonthly = mkRetentionOption "monthly";
                  keepYearly = mkRetentionOption "yearly";

                  extraConfig = extraConfigOption;
                };

                consistency = {
                  checks = mkOption {
                    type = types.listOf consistencyCheckModule;
                    default = [ ];
                    description = "Consistency checks to run";
                    example = literalExpression ''
                      [
                          {
                              name = "repository";
                              frequency = "2 weeks";
                          }
                          {
                              name = "archives";
                              frequency = "4 weeks";
                          }
                          {
                              name = "data";
                              frequency = "6 weeks";
                          }
                          {
                              name = "extract";
                              frequency = "6 weeks";
                          }
                      ];
                    '';
                  };
                  extraConfig = extraConfigOption;
                };
                hooks = {
                  beforePrune = mkOption {
                    type = types.listOf str;
                    default = [ ];
                    description = ''
                      List of one or more shell commands or scripts to execute
                      before pruning, run once per configuration file.
                    '';
                    example =
                      literalExpression "[ ''echo \"Starting pruning.\"'' ]";
                  };
                  beforeCheck = mkOption {
                    type = types.listOf str;
                    default = [ ];
                    description = ''
                      List of one or more shell commands or scripts to execute
                      before consistency checks, run once per configuration file.
                    '';
                    example =
                      literalExpression "[ ''echo \"Starting checks.\"'' ]";
                  };
                  beforeExtract = mkOption {
                    type = types.listOf str;
                    default = [ ];
                    description = ''
                      List of one or more shell commands or scripts to execute
                      before extracting a backup, run once per configuration file.
                    '';
                    example =
                      literalExpression "[ ''echo \"Starting extracting.\"'' ]";
                  };
                  afterBackup = mkOption {
                    type = types.listOf str;
                    default = [ ];
                    description = ''
                      List of one or more shell commands or scripts to execute
                      after creating a backup, run once per configuration file.
                    '';
                    example =
                      literalExpression "[ ''echo \"Finished a backup.\"'' ]";
                  };
                  afterPrune = mkOption {
                    type = types.listOf str;
                    default = [ ];
                    description = ''
                      List of one or more shell commands or scripts to execute
                      after pruning, run once per configuration file.
                    '';
                    example =
                      literalExpression "[ ''echo \"Finished pruning.\"'' ]";
                  };
                  afterCheck = mkOption {
                    type = types.listOf str;
                    default = [ ];
                    description = ''
                      List of one or more shell commands or scripts to execute
                      after consistency checks, run once per configuration file.
                    '';
                    example =
                      literalExpression "[ ''echo \"Finished checks.\"'' ]";
                  };
                  afterExtract = mkOption {
                    type = types.listOf str;
                    default = [ ];
                    description = ''
                      List of one or more shell commands or scripts to execute
                      after extracting a backup, run once per configuration file.
                    '';
                    example =
                      literalExpression "[ ''echo \"Finished extracting.\"'' ]";
                  };
                  onError = mkOption {
                    type = types.listOf str;
                    default = [ ];
                    description = ''
                      List of one or more shell commands or scripts to execute
                      when an exception occurs during a "prune", "create", or
                      "check" action or an associated before/after hook.
                    '';
                    example = literalExpression
                      "[ ''echo \"Error during prune/create/check.\"'' ]";
                  };
                  beforeEverything = mkOption {
                    type = types.listOf str;
                    default = [ ];
                    description = ''
                      List of one or more shell commands or scripts to execute
                      before running all actions (if one of them is "create").
                      These are collected from all configuration files and then
                      run once before all of them (prior to all actions).
                    '';
                    example =
                      literalExpression "[ ''echo \"Starting actions.\"'' ]";
                  };
                  afterEverything = mkOption {
                    type = types.listOf str;
                    default = [ ];
                    description = ''
                      List of one or more shell commands or scripts to execute
                      after running all actions (if one of them is "create").
                      These are collected from all configuration files and then
                      run once after all of them (after any action).
                    '';
                    example =
                      literalExpression "[ ''echo \"Completed actions.\"'' ]";
                  };
                  umask = mkOption {
                    type = types.str;
                    default = "0077";
                    description = ''
                      Umask used when executing hooks. Defaults to the umask that
                      borgmatic is run with.
                    '';
                    example = literalExpression "0077";
                  };
                  extraConfig = extraConfigOption;
                };
              };
            };
            removeNullValues = attrSet:
              filterAttrs (key: value: value != null) attrSet;

            writeConfig = config:
              generators.toYAML { } {
                location = removeNullValues {
                  source_directories = config.location.sourceDirectories;
                  repositories = config.location.repositories;
                } // config.location.extraConfig;
                storage = removeNullValues {
                  encryption_passcommand = config.storage.encryptionPasscommand;
                } // config.storage.extraConfig;
                retention = removeNullValues {
                  keep_within = config.retention.keepWithin;
                  keep_secondly = config.retention.keepSecondly;
                  keep_minutely = config.retention.keepMinutely;
                  keep_hourly = config.retention.keepHourly;
                  keep_daily = config.retention.keepDaily;
                  keep_weekly = config.retention.keepWeekly;
                  keep_monthly = config.retention.keepMonthly;
                  keep_yearly = config.retention.keepYearly;
                } // config.retention.extraConfig;
                consistency =
                  removeNullValues { checks = config.consistency.checks; }
                  // config.consistency.extraConfig;
                hooks = removeNullValues {
                  before_prune = config.hooks.beforePrune;
                  before_check = config.hooks.beforeCheck;
                  before_extract = config.hooks.beforeExtract;
                  after_backup = config.hooks.afterBackup;
                  after_prune = config.hooks.afterPrune;
                  after_check = config.hooks.afterCheck;
                  after_extract = config.hooks.afterExtract;
                  on_error = config.hooks.onError;
                  before_everything = config.hooks.beforeEverything;
                  after_everything = config.hooks.afterEverything;
                  umask =
                    config.storage.extraConfig.umask or config.hooks.umask;
                } // config.hooks.extraConfig;
              };
          in {
            options = {
              programs.borgmatic = {
                backups = mkOption {
                  type = types.attrsOf configModule;
                  description = ''
                    Borgmatic allows for several named backup configurations,
                    each with its own source directories and repositories.
                  '';
                  example = literalExpression ''
                    {
                        personal = {
                            location = {
                                sourceDirectories = [ "/home/me/personal" ];
                                repositories = [ "ssh://myuser@myserver.com/./personal-repo" ];
                            };
                        };
                        work = {
                            location = {
                                sourceDirectories = [ "/home/me/work" ];
                                repositories = [ "ssh://myuser@myserver.com/./work-repo" ];
                            };
                        };
                    };
                  '';
                };
              };
            };
            config = mkIf cfg.enable {
              xdg.configFile = with lib.attrsets;
                mapAttrs' (configName: config:
                  nameValuePair ("borgmatic.d/" + configName + ".yaml") {
                    text = writeConfig config;
                  }) cfg.backups;
            };
          };
        zoxide = { config, options, ... }:
          let cfg = config.programs.zoxide;
          in {
            options.programs.zoxide = {
              enableElvishIntegration = mkOption {
                default = true;
                type = types.bool;
                description = "Whether to enable Elvish integration.";
              };
            };
            config = let zoxide = "${cfg.package}/bin/zoxide";
            in mkIf cfg.enable (mkMErge (flatten [
              (mkIf cfg.enableElvishIntegration {
                programs.elvish.initExtra =
                  "eval (${zoxide} init elvish | slurp)";
              })
              (mkIf cfg.enableXonshIntegration {
                programs.xonsh.initExtra =
                  "execx($(${zoxide} init xonsh), 'exec', __xonsh__.ctx, filename='zoxide')";
              })
            ]));
          };
        starship = { config, options, ... }:
          let cfg = config.programs.starship;
          in {
            options.programs.starship = {
              enableElvishIntegration = mkOption {
                default = true;
                type = types.bool;
                description = "Whether to enable Elvish integration.";
              };
            };
            config = let starship = "${cfg.package}/bin/starship";
            in mkIf cfg.enable (mkMErge (flatten [
              (mkIf cfg.enableElvishIntegration {
                programs.elvish.initExtra = "eval (${starship} init elvish)";
              })
              (mkIf cfg.enableXonshIntegration {
                programs.xonsh.initExtra = "execx($(${starship} init xonsh))";
              })
            ]));
          };
        direnv = { config, options, ... }:
          let cfg = config.programs.direnv;
          in {
            options.programs.direnv = {
              enableElvishIntegration = mkOption {
                default = true;
                type = types.bool;
                description = "Whether to enable Elvish integration.";
              };
              enableXonshIntegration = mkOption {
                default = true;
                type = types.bool;
                description = "Whether to enable Xonsh integration.";
              };
            };
            config = mkIf cfg.enable (mkMErge (flatten [
              (mkIf cfg.enableElvishIntegration {
                xdg.configFile."elvish/direnv.elv".text =
                  "${direnv} hook elvish";
                programs.elvish.initExtra = "use direnv";
              })
              (mkIf cfg.enableXonshIntegration {
                programs.xonsh = {
                  packages = toList "xonsh-direnv";
                  initExtra = "xontrib load direnv";
                };
              })
            ]));
          };
        spacevim = { config, options, ... }:
          let
            cfg = config.programs.spacevim;
            Plugin = with types;
              { config, options, name, ... }: {
                options = {
                  enable = mkSubmoduleEnableOption name;
                  user = mkOption { type = str; };
                  repo = mkOption { type = str; };
                  merged = mkOption {
                    type = bool;
                    default = false;
                  };
                };
              };
            mkAutoloads = n:
              mapAttrs' (n: source:
                nameValuePair
                ".SpaceVim.d/autoload/${n}.${optionalString n "n"}vim" {
                  inherit source;
                }) cfg."${optionalString n "neo"}vimAutoloads";
          in {
            options.programs.spacevim = {
              enable = mkEnableOption "SpaceVim";
              extraConfig = mkOption {
                type = types.lines;
                default = "";
                description = "";
              };
              extraInit = mkOption {
                type = types.lines;
                default = "";
                description = "";
              };
              extraTOML = mkOption {
                type = types.attrs;
                default = { };
                description = "";
              };
              extraTOMLInit = mkOption {
                type = types.attrs;
                default = { };
                description = "";
              };
              enableNeovimIntegration = mkOption {
                type = types.bool;
                default = true;
                description = "";
              };
              enableVimIntegration = mkOption {
                type = types.bool;
                default = true;
                description = "";
              };
              plugins = mkOption {
                type = with types; listOf (either Plugin str);
                default = [ ];
                description = ''
                  A list of submodules / attribute sets comprising of the whether to enable the plugin,
                  the github user, the github repository name, and whether to merge the plugin.
                  Strings of the format <code>"[user]/[reponame]"</code> can also be used,
                  where the option to merge is <code>true</code> by default.
                '';
              };
              layers = mkOption {
                type = with types; listOf attrs;
                default = [ ];
                description = "";
              };
              options = mkOption {
                type = types.attrs;
                default = { };
                description = "";
              };
              vimAutoloads = mkOption {
                types = with types; attrsOf SourceText;
                default = { };
                description = "";
              };
              neovimAutoloads = mkOption {
                types = with types; attrsOf SourceText;
                default = { };
                description = "";
              };
            };
            config = mkIf cfg.enable (mkMerge (flatten [
              {
                home.file.".SpaceVim.d/init.toml".text = mkBefore cfg.extraInit;
              }
              (let optStrings = flatten [ (toTOML cfg.extraTOMLInit) ];
              in imap1r (i: v: {
                home.file.".SpaceVim.d/init.toml".text = mkOrder (1000 - 1) v;
              }) optStrings)
              {
                home.file.".SpaceVim.d/init.toml".text = toTOML {
                  custom_plugins = map (repo:
                    if (isAttrs repo) then {
                      inherit (repo) merged;
                      repo = "${repo.user}/${repo.repo}";
                    } else {
                      inherit repo;
                      merged = true;
                    }) cfg.plugins;
                  inherit (cfg) layers options;
                };
              }
              {
                home.file = mkMerge [ (mkAutoloads false) (mkAutoloads true) ];
              }
              {
                home.file.".SpaceVim.d/init.toml".text =
                  mkAfter cfg.extraConfig;
              }
              {
                programs = let plugins = toList pkgs.vimPlugins.SpaceVim;
                in {
                  neovim.plugins = mkIf cfg.enableNeovimIntegration plugins;
                  vim.plugins = mkIf cfg.enableVimIntegration plugins;
                };
              }
              (let optStrings = flatten [ (toTOML cfg.extraTOML) ];
              in imap1 (i: v: {
                home.file.".SpaceVim.d/init.toml".text = mkOrder (1500 + i) v;
              }) optStrings)
            ]));
          };
        qtile = { config, options, ... }:
          let
            cfg = config.programs.qtile;
            Mouse = with types;
              submodule {
                options = {
                  function = mkOption {
                    type = str;
                    description = "";
                  };
                  args = mkOption {
                    type = list;
                    default = [ ];
                    description = "";
                  };
                  kwargs = mkOption {
                    type = attrs;
                    default = { };
                    description = "";
                  };
                };
              };
            Layout = with types;
              submodule {
                options = {
                  function = mkOption {
                    type = str;
                    description = "";
                  };
                  args = mkOption {
                    type = list;
                    default = [ ];
                    description = "";
                  };
                  kwargs = mkOption {
                    type = attrs;
                    default = { };
                    description = "";
                  };
                };
              };
            Group = with types;
              submodule {
                options = {
                  name = mkOption {
                    type = either str (functionTo str);
                    description = "The name of the group.";
                  };
                  matches = mkOption {
                    type = listOf attrs;
                    default = [ ];
                    description =
                      "A list of attribute sets / modules of the <code>Match</code> class in <command>qtile</command>.";
                  };
                };
              };
            Key = with types;
              submodule {
                options = {
                  mod = mkOption {
                    type = let strs = [ str (functionTo str) ];
                    in nullOr (oneOf (flatten [ strs (listOf (oneOf strs)) ]));
                    default = null;
                    description = ''
                      The modifier combo to use, such as <code>mod "shift"</code>, etc.

                      If <code>null</code> (the default value), the value of <code>services.qtile.mod</code> will be used.
                    '';
                  };
                  addMod = mkOption {
                    type = bool;
                    default = true;
                    description =
                      "Whether to prepend <code>services.qtile.mod</code> to the modifier keys.";
                  };
                  key = mkOption {
                    type = either str (functionTo str);
                    description = "The key portion of the keybinding.";
                  };
                  action = mkOption {
                    type = str;
                    description =
                      "The function to run on activating this keybinding.";
                  };
                };
              };
          in {
            options.services.qtile = {
              enable = mkEnableOption "Qtile";
              package = mkPackageOption pkgs "qtile" { };
              python3Package = mkOption {
                type = types.package;
                default = pkgs.python3;
                defaultText = literalExpression "pkgs.python3";
                description =
                  "The Python 3 package to be used for the <code>python3Packages</code> attribute.";
              };
              python3Packages = mkOption {
                type = with types; nullOr (attrsOf package);
                default = null;
                description = "The Python 3 packages attribute set.";
              };
              packages = mkOption {
                type = with types; listOf (either nonEmtpyStr package);
                default = [ ];
                description =
                  "A list of packages or package names for <command>qtile</command> to install.";
              };
              extraInit = mkOption {
                default = "";
                type = types.lines;
                description =
                  "Extra commands that should be run at the very beginning of initializing a <command>qtile</command> session.";
              };
              extraConfig = mkOption {
                default = "";
                type = types.lines;
                description =
                  "Extra commands that should be run when initializing a <command>qtile</command> session.";
              };
              imports = mkOption {
                type = with types; listOf nonEmptyStr;
                default = [ ];
                description =
                  "List of packages to be imported into the <command>qtile</command> config.";
              };
              fromImports = mkOption {
                type = with types; attrsOf (listOf nonEmptyStr);
                default = { };
                description =
                  "Attribute set of features to be imported into the <command>qtile</command> config from different packages.";
              };
              functions = mkOption {
                type = types.lines;
                default = "";
                description = "Functions to include.";
              };
              mod = mkOption {
                type = with types; either str (functionTo str);
                default = "mod4";
                description = "Modifier key for <command>qtile</command>.";
              };
              keys = mkOption {
                type = types.listOf Key;
                default = [ ];
                description = ''
                  A list of attribute sets / modules of keybindings,
                  comprised of the modifier keys, the keys, and the function to run.
                '';
              };
              groups = mkOption {
                type = types.listOf Group;
                default = [ ];
                description = ''
                  A list of attribute sets / modules of groups,
                  comprised of the names and a list of matches of the Match module.
                '';
              };
              dgroupsKeyBinder = mkOption {
                type = with types;
                  submodule {
                    options = {
                      enable = mkSubmoduleEnableOption "dgroupsKeyBinder";
                      key = mkOption {
                        type = oneOf [ null str (functionTo str) ];
                        default = null;
                        description = "";
                      };
                    };
                  };
                default.enable = true;
                description = "";
              };
              layouts = mkOption {
                type = types.listOf Layout;
                default = [ ];
                description = ''
                  A list of attribute sets / modules of layouts,
                  comprised of the <code>layout</code> function to be used and any keyword arguments that the function can take.
                '';
              };
              variables = mkOption {
                type = types.attrs;
                default = { };
                description = "";
              };
              mouse = mkOption {
                type = types.listOf Mouse;
                default = [ ];
                description = ''
                  A list of attribute sets / modules of mouse events,
                  comprised of the <code>mouse</code> event function to be used and any keyword arguments that the function can take.
                '';
              };
            };
            config = let
              mkWithPackages = pkg: pkglist: pname:
                (pkg.override {
                  inherit (cfg) python3;
                  python3Packages = with cfg;
                    if (python3Packages == null) then
                      cfg.pkgs
                    else
                      python3Packages;
                }).overrideAttrs (old: {
                  propagatedBuildInputs = flatten [
                    (bundle.filters.has.list cfg.packages python3Packages)
                    (old.propagatedBuildInputs or [ ])
                  ];
                });
              imports = toList cfg.imports;
              fromImports = mapAttrs (n: toList)
                (bundle.attrTree.attrsRemoveNull cfg.fromImports);
            in mkIf cfg.enable (mkMerge (flatten [
              {
                services.qtile = mkBefore {
                  dgroupsKeyBinder.key = mkIf (cfg.dgroupsKeyBinder.enable
                    && (cfg.dgroupsKeyBinder.key == null)) cfg.mod;
                  package = (if (cfg.package ? withPackages) then
                    inputs.bundle.lib.bundle.mkWithPackages
                  else
                    mkWithPackages) cfg.package packages null;
                };
              }
              { home.packages = toList cfg.package; }
              (let
                optStrings = flatten [
                  cfg.extraInit
                  (optionalString (imports != [ ])
                    ("import " + (concatStringsSep ", " imports)))
                  (map newcat [
                    (mapAttrsToList (n: v:
                      optionalString (v != [ ])
                      "from ${n} import ${concatStringsSep ", " v}")
                      fromImports)
                    (mapAttrsToList convertAssignment cfg.variables)
                  ])
                  cfg.functions
                  (optionalString (cfg.mod != "") "mod = ${cfg.mod}")
                  (let
                    Keys = map (k:
                      let
                        kmod = if (k.mod == null) then
                          (f: "mod")
                        else if (isList k.mod) then
                          (flatten [
                            (optional ((!(elem null k.mod)) && k.addMod)
                              (f: "mod"))
                            (map (m: if (m == null) then (f: "mod") else m)
                              k.mod)
                          ])
                        else if k.addMod then [
                          (f: "mod")
                          k.mod
                        ] else
                          k.mod;
                      in ''
                        Key(
                            ${convert (toList kmod)}, ${convert k.key},
                            ${k.action},
                        ),
                      '') cfg.keys;
                  in optionalString (cfg.keys != [ ]) ''
                    keys = [
                        ${newcat Keys}
                    ]
                  '')
                  (let
                    Groups = map (g:
                      let
                        gName = if (isString g) then g else g.name;
                        gMatches = optionalString ((isAttrs g) && (g ? matches))
                          (let
                            matches = map (m:
                              concatStringsSep ", "
                              (mapAttrsToList convertAssignment m)) g.matches;
                            Matches = concatStringsSep "), Match(" matches;
                          in ", matches = [ Match(${Matches}) ]");
                      in "Group(${gName}${gMatches})") cfg.groups;
                  in optionalString (cfg.groups != [ ]) ''
                    groups = [
                        ${newcat Groups}
                    ]
                  '')
                  (optionalString cfg.dgroupsKeyBinder.enable
                    "dgroups_key_binder = simple_key_binder(${
                      convert cfg.dgroupsKeyBinder.key
                    })")
                  (let
                    Layouts = map (l:
                      let
                        args = concatStringsSep ", " (map convert l.args);
                        kwargs = optionalString (l.kwargs != { }) (", "
                          + (concatStringsSep ", "
                            (mapAttrsToList convertAssignment l.kwargs)));
                      in "layout.${l.function}(${args}${kwargs})") cfg.layouts;
                  in optionalString (cfg.layouts != [ ]) ''
                    layouts = [
                        ${newcat Layouts}
                    ]
                  '')
                  (let
                    Mice = map (m:
                      let
                        args = concatStringsSep ", " (map convert m.args);
                        kwargs = optionalString (m.kwargs != { }) (", "
                          + (concatStringsSep ", "
                            (mapAttrsToList convertAssignment m.kwargs)));
                      in "${m.function}(${args}${kwargs})") cfg.mouse;
                  in optionalString (cfg.mouse != [ ]) ''
                    mouse = [
                        ${newcat Mice}
                    ]
                  '')
                ];
              in bundle.imap1r (i: v: {
                xdg.configFile."qtile/config.py".text = mkOrder (1000 - i) v;
              }) optStrings)
              {
                xdg.configFile."qtile/config.py".text = mkAfter cfg.extraConfig;
              }
            ]));
          };
        xonsh = { config, options, ... }:
          let
            cfg = config.programs.xonsh;
            code = with types;
              submodule {
                options.code = mkOption {
                  type = nonEmptyStr;
                  description =
                    "A shell alias that will be treated as code, and not be quoted.";
                };
              };
          in {
            options.programs.xonsh = {
              enable = mkEnableOption "Xonsh";
              package = mkPackageOption pkgs "xonsh" { };
              python3Packages = mkOption {
                type = with types; attrsOf package;
                default = pkgs.python3Packages;
                description = "The Python 3 packages attribute set.";
              };
              packages = mkOption {
                type = with types; listOf (either nonEmtpyStr package);
                default = [ ];
                description =
                  "A list of packages or package names for <command>xonsh</command> to install.";
              };
              initExtra = mkOption {
                default = "";
                type = types.lines;
                description =
                  "Extra commands that should be run when initializing an interactive shell.";
              };
              imports = mkOption {
                type = with types; listOf nonEmptyStr;
                default = [ ];
                description =
                  "List of packages to be imported into the <command>xonsh</command> config.";
              };
              fromImports = mkOption {
                type = with types; attrsOf (listOf nonEmptyStr);
                default = { };
                description =
                  "Attribute set of features to be imported into the <command>xonsh</command> config from different packages.";
              };
              sessionVariables = mkOption {
                default = { };
                type = types.attrs;
                example = {
                  MAILCHECK = 30;
                  CWD = f: "Path.cwd()";
                };
                description =
                  "Environment variables that will be set for the Xonsh session.";
              };
              sessionVariablesAppend = mkOption {
                default = { };
                type = types.attrs;
                example = {
                  MAILCHECK = 30;
                  CWD = f: "Path.cwd()";
                };
                description =
                  "Environment variables that will be added to existing environment variables for the Xonsh session.";
              };
              sessionPath = mkOption {
                type = with types; listOf (either nonEmptyStr code);
                default = [ ];
                example = [
                  "$HOME/.local/bin"
                  "\${xdg.configHome}/emacs/bin"
                  ".git/safe/../../bin"
                  { code = "Path.cwd()"; }
                ];
                description = ''
                  Extra directories to add to <envar>PATH</envar>.
                  </para><para>
                  These directories are added to the <envar>PATH</envar> variable in a
                  double-quoted context, so expressions like <code>$HOME</code> are
                  expanded by the shell. However, since expressions like <code>~</code> or
                  <code>*</code> are escaped, they will end up in the <envar>PATH</envar>
                  verbatim.
                '';
              };
              shellAliases = mkOption {
                default = { };
                type = with types; attrsOf (either str (functionTo str));
                example = literalExpression ''
                  {
                      ll = "ls -l";
                      ".." = "cd ..";
                  }
                '';
                description = ''
                  An attribute set that maps aliases (the top level attribute names in
                  this option) to command strings or directly to build outputs.
                '';
              };
              functions = mkOption {
                type = types.lines;
                default = "";
                description = "Functions to include.";
              };
              xontribs = mkOption {
                type = with types; listOf nonEmptyStr;
                default = [ ];
                description = "Xontribs to enable.";
              };
            };
            config = let
              mkWithPackages = pkg: pkglist: pname:
                (pkg.override { inherit (cfg) python3Packages; }).overrideAttrs
                (old: {
                  propagatedBuildInputs = flatten [
                    (bundle.filters.has.list cfg.packages python3Packages)
                    (old.propagatedBuildInputs or [ ])
                  ];
                });
              imports = toList cfg.imports;
              fromImports = mapAttrs (n: toList)
                (bundle.attrTree.attrsRemoveNull cfg.fromImports);
            in mkIf cfg.enable (mkMerge (flatten [
              {
                programs.xonsh = mkBefore {
                  package = (if (cfg.package ? withPackages) then
                    inputs.bundle.lib.bundle.mkWithPackages
                  else
                    mkWithPackages) cfg.package packages null;
                };
              }
              (let
                optStrings = flatten [
                  (optionalString (imports != [ ])
                    ("import " + (concatStringsSep ", " imports)))
                  (map newcat [
                    (mapAttrsToList (n: v:
                      optionalString (v != [ ])
                      "from ${n} import ${concatStringsSep ", " v}")
                      fromImports)
                    (mapAttrsToList (convertAttrs false) cfg.sessionVariables)
                    (mapAttrsToList (convertAttrs true)
                      cfg.sessionVariablesAppend)
                  ])

                  # TODO: Do I need this line: `sys.path.insert(0, "")'
                  (optionalString (cfg.sessionPath != [ ]) ''
                    $PATH += [ "${concatStringsSep ''", "'' cfg.sessionPath}" ]
                  '')

                  cfg.functions
                  (map newcat [
                    (map (x: "xontrib load" + x) cfg.xontribs)
                    (mapAttrsToList (n: v: ''aliases["${n}"] = ${convert v}'')
                      cfg.shellAliases)
                  ])
                ];
              in bundle.imap1r
              (i: v: { home.file.".xonshrc".text = mkOrder (1000 - i) v; })
              optStrings)
              {
                home = {
                  file.".xonshrc".text = mkAfter cfg.initExtra;
                  packages = toList cfg.package;
                };
              }
            ]));
          };
        elvish = { config, options, ... }:
          let
            cfg = config.programs.elvish;
            noldor = { config, name, ... }:
              with types; {
                options = {
                  enable = mkSubmoduleEnableOption name;
                  url = mkOption {
                    type = nonEmptyStr;
                    description = "The elvish package to install using `epm'";
                  };
                  use = mkOption {
                    type = nullOr nonEmptyStr;
                    description = ''
                      The file to use from ${name}'s package.

                      If empty, the name of the package will be used.
                    '';
                  };
                };
              };
          in {
            options.programs.elvish = {
              enable = mkEnableOption "Elvish";
              sessionVariables = mkOption {
                default = { };
                type = types.attrs;
                example = { MAILCHECK = 30; };
                description = ''
                  Environment variables that will be set for the Bash session.
                '';
              };
              sessionPath = mkOption {
                type = with types; listOf nonEmptyStr;
                default = [ ];
                example = [
                  "$HOME/.local/bin"
                  "\${xdg.configHome}/emacs/bin"
                  ".git/safe/../../bin"
                ];
                description = ''
                  Extra directories to add to <envar>PATH</envar>.
                  </para><para>
                  These directories are added to the <envar>PATH</envar> variable in a
                  double-quoted context, so expressions like <code>$HOME</code> are
                  expanded by the shell. However, since expressions like <code>~</code> or
                  <code>*</code> are escaped, they will end up in the <envar>PATH</envar>
                  verbatim.
                '';
              };
              pathCheck = mkOption {
                type = types.bool;
                default = true;
                description =
                  "Whether to check if paths have disappeared when opening a new shell.";
              };
              shellAliases = mkOption {
                default = { };
                type = with types; attrsOf str;
                example = literalExpression ''
                  {
                      ll = "ls -l";
                      ".." = "cd ..";
                  }
                '';
                description = ''
                  An attribute set that maps aliases (the top level attribute names in
                  this option) to command strings or directly to build outputs.
                '';
              };
              package = mkPackageOption pkgs "elvish" { };
              initExtra = mkOption {
                default = "";
                type = types.lines;
                description =
                  "Extra commands that should be run when initializing an interactive shell.";
              };
              lib = mkOption {
                type = with types; attrsOf (submodule SourceText);
                description = "";
                default = { };
              };
              use = mkOption {
                type = with type; listOf nonEmptyStr;
                description = "";
                default = [ ];
              };
              evaluations = mkOption {
                type = with types; listOf nonEmptyStr;
                default = [ ];
                description = "Evaluations to be run.";
              };
              epm = mkOption {
                type = with types; attrsOf (submodule noldor);
                description = "";
                default = { };
              };
              silenceEPM = mkOption {
                type = types.bool;
                description =
                  "Silence <command>epm</command> if a package is already installed.";
                default = true;
              };
            };
            config = mkIf cfg.enable (mkMerge (flatten [
              { home.packages = toList cfg.package; }
              {
                xdg.configFile = mapAttrs' (n: v:
                  nameValuePair "elvish/lib/${n}.elv" { inherit (v) source; })
                  cfg.lib;
              }
              (let
                optStrings = flatten [
                  (map newcat [
                    (map (u: "use " + u) cfg.use)
                    (mapAttrsToList (n: v:
                      optionalString v.enable ''
                        epm:install ${
                          optionalString cfg.silenceEPM "&silent-if-installed"
                        } ${v.url}
                        use ${v.url}/${if (v.use == null) then n else v.use}
                      '') cfg.epm)
                    (mapAttrsToList (n: v: optionalString v.enable "use ${n}")
                      cfg.lib)
                    (mapAttrsToList (n: v: "E:${n} = ${v}")
                      cfg.sessionVariables)
                  ])
                  (optionalString (cfg.sessionPath != [ ])
                    "path = [ ${toString cfg.sessionPath} ]")
                  (optionalString cfg.pathCheck ''
                    each [p]{
                        if (not (path:is-dir &follow-symlink $p)) {
                            echo (styled "Warning: directory "$p" in $paths no longer exists." red)
                        }
                    } $paths
                  '')
                  (optionalString (cfg.epm != { }) "use epm")
                  (map newcat [
                    (mapAttrsToList (n: v:
                      optionalString v.enable ''
                        epm:install ${v.url}
                        use ${v.url}/${if (v.use == null) then n else v.use}
                      '') cfg.epm)
                  ])
                  (optionalString (cfg.shellAliases != { }) ''
                    epm:install github.com/zzamboni/elvish-modules
                    use github.com/zzamboni/elvish-modules/alias
                  '')
                  (newCat (mapAttrsToList (n: v: "alias:new ${n} ${v}")
                    cfg.shellAliases))
                ];
              in bundle.imap1r (i: v: {
                xdg.configFile."elvish/rc.elv".text = mkOrder (1000 - i) v;
              }) optStrings)
              { xdg.configFile."elvish/rc.elv".text = mkBefore cfg.initExtra; }
              {
                xdg.configFile."elvish/rc.elv".text = mkAfter
                  (newcat (map (e: ''eval "$(${e})"'') cfg.evaluations));
              }
            ]));
          };
        bash = { config, options, ... }:
          let cfg = config.programs.bash;
          in {
            options.programs.bash = {
              evaluations = mkOption {
                type = with types; listOf nonEmptyStr;
                default = [ ];
                description = "Evaluations to be run.";
              };
            };
            config = mkIf cfg.enable (mkMerge (flatten [{
              programs.bash.initExtra =
                mkAfter (newcat (map (e: ''eval "$(${e})"'') cfg.evaluations));
            }]));
          };
        zsh = { config, options, ... }:
          let cfg = config.programs.zsh;
          in {
            options.programs.zsh = {
              evaluations = mkOption {
                type = with types; listOf nonEmptyStr;
                default = [ ];
                description = "Evaluations to be run.";
              };
              shellOptions = mkOption {
                type = with types; listOf nonEmptyStr;
                default = [ ];
                description = "Options to be set.";
              };
            };
            config = mkIf cfg.enable (mkMerge (flatten [
              {
                programs.zsh.initExtra = mkAfter
                  (newcat (map (e: ''eval "$(${e})"'') cfg.evaluations));
              }
              (let
                optStrings =
                  flatten [ (map (sepcat "setopt") cfg.shellOptions) ];
              in imap1
              (i: v: { programs.zsh.initExtra = mkOrder (1500 + i) v; })
              optStrings)
            ]));
          };
        tmux = { config, options, ... }:
          let cfg = config.programs.tmux;
          in {
            options.programs.tmux = {
              displayTime = mkOption {
                type = types.ints.positive;
                default = 750;
                example = 1250;
                description = "Amount of time to display tmux messages.";
              };
              extraInit = mkOption {
                type = types.lines;
                default = "";
                description = ''
                  Initial configuration to add to
                  <filename>tmux.conf</filename>.
                '';
              };
              sources = mkOption {
                type = with types; listOf (either nonEmptyStr path);
                default = [ ];
                description = ''
                  List of files to source.

                  Sourced after <code>config.programs.tmux.extraInit</code>.
                '';
              };
              shells-to-run = mkOption {
                type = with types; listOf nonEmptyStr;
                default = [ ];
                description = ''
                  List of shells to run.

                  Run after <code>config.programs.tmux.sources</code>.
                '';
              };
              unbindings = mkOption {
                type = with types; listOf nonEmptyStr;
                default = [ ];
                description = "List of keybindings to unbind.";
              };
              bindings = mkOption {
                type = with types; attrsOf nonEmptyStr;
                default = { };
                description = "Attribute set of keybindings to bind.";
              };
              root-bindings = mkOption {
                type = with types; attrsOf nonEmptyStr;
                default = { };
                description = "Attribute set of root keybindings to bind.";
              };
              repeating-bindings = mkOption {
                type = with types; attrsOf nonEmptyStr;
                default = { };
                description = "Attribute set of repeating keybindings to bind.";
              };
              root-repeating-bindings = mkOption {
                type = with types; attrsOf nonEmptyStr;
                default = { };
                description =
                  "Attribute set of root repeating keybindings to bind.";
              };
              prefix-table-bindings = mkOption {
                type = with types; attrsOf nonEmptyStr;
                default = { };
                description =
                  "Attribute set of prefix table keybindings to bind.";
              };
              set = mkOption {
                type = with types; attrsOf nonEmptyStr;
                default = { };
                description = "Attribute set of options to set.";
              };
              set-window = mkOption {
                type = with types; attrsOf nonEmptyStr;
                default = { };
                description = "Attribute set of window options to set.";
              };
              set-server = mkOption {
                type = with types; attrsOf nonEmptyStr;
                default = { };
                description = "Attribute set of server options to set.";
              };
              set-globally = mkOption {
                type = with types; attrsOf nonEmptyStr;
                default = { };
                description = "Attribute set of options to set globally.";
              };
              set-window-globally = mkOption {
                type = with types; attrsOf nonEmptyStr;
                default = { };
                description =
                  "Attribute set of window options to set globally.";
              };
              set-server-globally = mkOption {
                type = with types; attrsOf nonEmptyStr;
                default = { };
                description =
                  "Attribute set of server options to set globally.";
              };
              set-and-append = mkOption {
                type = with types; attrsOf nonEmptyStr;
                default = { };
                description = "Attribute set of options to append to.";
              };
              set-and-append-window = mkOption {
                type = with types; attrsOf nonEmptyStr;
                default = { };
                description = "Attribute set of window options to append to.";
              };
              set-and-append-server = mkOption {
                type = with types; attrsOf nonEmptyStr;
                default = { };
                description = "Attribute set of server options to append to.";
              };
              set-and-append-globally = mkOption {
                type = with types; attrsOf nonEmptyStr;
                default = { };
                description = "Attribute set of options to append to globally.";
              };
              set-and-append-window-globally = mkOption {
                type = with types; attrsOf nonEmptyStr;
                default = { };
                description =
                  "Attribute set of window options to append to globally.";
              };
              set-and-append-server-globally = mkOption {
                type = with types; attrsOf nonEmptyStr;
                default = { };
                description =
                  "Attribute set of server options to append to globally.";
              };
            };
            config = mkIf cfg.enable {
              xdg.configFile."tmux/tmux.conf".text = let
                concatList = with cfg; {
                  source = cfg.sources;
                  unbind = unbindings;
                  run-shell = shells-to-run;
                };
                concatSet = with cfg; {

                  bind-key = bindings;
                  "bind-key -n" = root-bindings;
                  "bind-key -r" = repeating-bindings;
                  "bind-key -nr" = root-repeating-bindings;
                  "bind-key -T prefix" = prefix-table-bindings;
                  inherit set;
                  "set -w" = set-window;
                  "set -s" = set-server;
                  "set -g" = set-globally;
                  "set -wg" = set-window-globally;
                  "set -sg" = set-server-globally;
                  "set -a" = set-and-append;
                  "set -aw" = set-and-append-window;
                  "set -as" = set-and-append-server;
                  "set -ag" = set-and-append-globally;
                  "set -agw" = set-and-append-window-globally;
                  "set -ags" = set-and-append-server-globally;
                };
              in mkOrder 499 (newcat (flatten [
                cfg.extraInit
                "set -g ${toString cfg.displayTime}"
                (mapAttrsToList (n: map (v: "${n} ${toString v}")) concatList)
                (mapAttrsToList (command:
                  mapAttrsToList (n: v: "${command} ${n} ${toString v}"))
                  concatSet)
              ]));
            };
          };
      };
      nixosModules = {
        persistence = { config, pkgs, ... }: {
          environment.persistence = let
            reallyUnique = list:
              let
                attrs = remove null (map (item:
                  if (isAttr item) then (item.file or item.directory) else null)
                  list);
              in unique (filter (item: !(elem item attrs)) list);
            mergePersisted = set: list:
              reallyUnique (map (item:
                if (isString item) then
                  (recursiveUpdate { inherit item; } set)
                else
                  (recursiveUpdate set item)) (flatten list));
          in {
            "/persist/root" = let
              rootDirSet = {
                user = "root";
                group = "root";
              };
              rootFileSet.parentDirectory = rootDirSet;
              etc-prefixes = [
                "nixos"
                "containers"
                "NetworkManager/system-connections"
                "tailscale"
              ];
            in {
              hideMounts = true;
              files = mergePersisted rootFileSet [
                "/etc/host"
                "/etc/machine-id"
                (map (directory:
                  bundle.imports.list {
                    dir = "${inputs.user}/${directory}";
                    root = true;
                    file.prefix.post = "/${directory}";
                    ignores = {
                      prefix = etc-prefixes;
                      dirs = true;
                    };
                    recursive = true;
                  }) [ "etc" "var" ])
              ];
              directories = mergePersisted rootDirSet [
                (map (d: "/etc/" + d) etc-prefixes)

                "/bin"

                # TODO: Prevents `sshd_config' itself from being created
                # "/etc/ssh"

                "/sbin"
                "/snap"
                "/usr"
                "/var/lib/acme"
                "/var/lib/bluetooth"
                "/var/lib/systemd/coredump"
                "/var/log"

                # Managed by the `var' module
                # "/var/lib/tailscale"
              ];
            };
            "/persist" = let
              redRepoFiles = flatten [ (bundle.dirCon.others inputs.user) ];
              redRepoDirectories = flatten [ (bundle.dirCon.dirs inputs.user) ];
            in {
              users = mapAttrs' (designation: user:
                let
                  home = bundle.attrs.allHomes.${designation};
                  pHome = "/persist/${home}";
                  userDirSet = {
                    inherit user;
                    group = user;
                  };
                  userFileSet.parentDirectory = userDirSet;
                  predRepoFiles = flatten [ (bundle.dirCon.others pHome) ];
                  predRepoDirectories = flatten [ (bundle.dirCon.dirs pHome) ];
                in nameValuePair user {
                  inherit home;
                  files = mergePersisted userFileSet [
                    ".bash_history"
                    ".emacs-profile"
                    ".fasd"
                    ".gitignore"
                    ".globalignore"
                    ".nix-channels"
                    ".python-history"
                    ".screenrc"
                    ".viminfo"
                    ".zsh_history"
                    config.services.caddy.dataDir
                    redRepoFiles
                    predRepoFiles
                  ];
                  directories = mergePersisted userDirSet [
                    ".atom"
                    ".byobu"
                    ".cache"
                    ".config"
                    ".linuxbrew"
                    ".local"
                    ".mozilla"
                    ".peru"
                    ".pki"
                    ".repos"
                    ".user"
                    ".vim_runtime"
                    ".virtualenvs"
                    ".vscode-oss"
                    ".vscode"
                    ".yubico"
                    ".z"
                    "Documents"
                    "Downloads"
                    "keybase"
                    "Music"
                    "nix-plugins"
                    "Pictures"
                    "Public"
                    "Templates"
                    "tests"
                    "Videos"
                    "VirtualBox VMs"
                    {
                      directory = ".gnupg";
                      mode = "0700";
                    }
                    {
                      directory = ".nixops";
                      mode = "0700";
                    }
                    {
                      directory = ".ssh";
                      mode = "0700";
                    }
                    {
                      directory = ".gnupgk";
                      mode = "0700";
                    }
                    redRepoDirectories
                    predRepoDirectories
                  ];
                }) bundle.attrs.allUsers;
            };
          };
        };
        borgmatic = { config, pkgs, ... }: {
          systemd = let
            replace = replaceStrings [ "borgmatic-" ] [ "" ];
            mkBaseWants = list:
              bundle.systemd.wants ([ "rclone-backblazeB2.service" ] ++ list);
            mkBase = n: rec {
              description = "Borgmatic ${(bundle.toCapital n)} Backup";
              unitConfig.ConditionACPower = "true";
              serviceConfig = {
                CPUSchedulingPolicy = "batch";
                ExecStart = let
                  inherit (config.home.${bundle.attrs.allUsers.primary}.xdg.configFile."borgmatic.d/${n}.yaml")
                    path;
                in ''
                  ${pkgs.systemd}/bin/systemd-inhibit --who=\"${description}\" \
                                                      --why=\"Prevent interrupting scheduled backup for `${description}'\" \
                                                      ${config.home.programs.brogmatic.package}/bin/borgmatic \
                                                      --verbosity -1 \
                                                      --syslog-verbosity 1 \
                                                      --config ${path}
                '';
                ExecStartPre = "${pkgs.coreutils}/bin/sleep 10m";
                IOSchedulingClass = "best-effort";
                IOSchedulingPriority = "7";
                IOWeight = "100";
                LockPersonality = "true";
                LogRateLimitIntervalSec = "0";
                MemoryDenyWriteExecute = "no";
                Nice = "19";
                NoNewPrivileges = "yes";
                PrivateDevices = "yes";
                PrivateTmp = "yes";
                ProtectClock = "yes";
                ProtectControlGroups = "yes";
                ProtectHome = "tmpfs";
                ProtectHostname = "yes";
                ProtectKernelLogs = "yes";
                ProtectKernelModules = "yes";
                ProtectKernelTunables = "yes";
                ProtectSystem = "strict";
                Restart = "no";
                RestrictAddressFamilies = "AF_UNIX AF_INET AF_INET6 AF_NETLINK";
                RestrictNamespaces = "yes";
                RestrictRealtime = "yes";
                RestrictSUIDSGID = "yes";
                SystemCallArchitectures = "native";
                SystemCallErrorNumber = "EPERM";
                SystemCallFilter = "@system-service";
                Type = "oneshot";
              };
            };
            borgmatic-services = bundle.mapAttrNames (n: v: "borgmatic-" + n) {
              oreo = rec {
                wants = mkBaseWants [ "chimchar-oreo.mount" "oreo.mount" ];
                after = wants;
                serviceConfig = {
                  ReadWritePaths = "-/oreo";
                  ReadOnlyPaths = "-/chimchar/oreo";
                };
              };
              oreo-rsync = rec {
                wants = mkBaseWants [ "chimchar-oreo.mount" ];
                after = wants;
                serviceConfig.ReadOnlyPaths = "-/chimchar/oreo";
              };
              chimchar = rec {
                wants = mkBaseWants [ "chimchar.mount" "infernape.mount" ];
                after = wants;
                serviceConfig = {
                  ReadWritePaths = "-/infernape";
                  ReadOnlyPaths = "-/chimchar";
                };
              };
              user = rec {
                wants = mkBaseWants [
                  "${
                    replaceStrings [ "/" ] [ "-" ] (removeSuffix "/"
                      (removePrefix "/" bundle.attrs.homes.primary))
                  }.mount"
                ];
                after = wants;
                serviceConfig = {
                  BindPaths =
                    "-/home/shadowrylander/aiern -${bundle.attrs.homes.primary}/.user";
                };
              };
            };
          in {
            services = mkMerge (flatten [
              (mapAttrs (n: v:
                bundle.fold.recursive [
                  bundle.attrs.configs.services.base
                  (mkBase (replace n))
                  v
                ]) borgmatic-services)
            ]);
          };
          timers = let
            mkBase = n: {
              description = "Borgmatic ${toCapital n} Backup Timer";
              wantedBy = [ "timers.target" ];
              timeConfig = {
                OnCalender = "*-*-* */6:00:00";
                Persistent = "true";
                RandomizedDelaySec = "3h";
              };
            };
          in mkMerge (flatten [
            (genAttrs (attrNames borgmatic-services) (t: mkBase (replace t)))
            (mapAttrs' (n: v:
              "borgmatic-${n}" (recursiveUpdate (mkBase (replace t)) v)) {
                user.timeConfig = {
                  OnCalender = "*-*-* */3:00:00";
                  RandomizedDelaySec = "90min";
                };
              })
          ]);
        };
        etc = args@{ config, system, ... }: {
          etc = let
            mountable-mounted = pkgs.writeShellScript "mountable-mounted" ''
              list=$(zfs list -rHo name,mountpoint,mounted $1) || exit 75
              list=($list)
              pm=3
              for ((i = 0; i <= ''${#list[@]} - $pm; i = i + $pm)); do
                  name=''${list[$i]}
                  mountpoint=''${list[$i + 1]}
                  mounted=''${list[$i + 2]}
                  if [[ "$mountpoint" != "none" ]] && [[ $mounted == "no" ]]; then
                      echo "Dataset \"$name\" is not mounted to \"$mountpoint\"."
                      exit 75
                  fi
              done
            '';
          in {
            "nix/nix.conf".text = mkForce bundle.attrs.configs.nix;
            "zsh/keephack".source = "${grml}/etc/zsh/keephack";
          };
        };
        home-manager = args@{ config, pkgs, ... }: {
          home-manager = {
            imports = attrValues self.homeModules;
            users = mkMerge [
              (mapAttrs' (user: designation:
                let
                  homeDirectory = bundle.attrs.allHomes.${designation};
                  cfg = config.home-manager.users.${user};
                  cfgx = cfg.xdg.configFile;
                  relToZshDir = cfg.programs.zsh.dotDir or "";
                  direnv = "${pkgs.direnv}/bin/direnv";
                  emacsclient = "${cfg.services.emacs.package}/bin/emacsclient";
                  fzf-tmux = "${cfg.programs.fzf.package}/bin/fzf-tmux";
                  fasd = "${pkgs.fasd}/bin/fasd";
                  zoxide = "${cfg.programs.zoxide.package}/bin/zoxide";
                  starship = "${cfg.programs.starship.package}/bin/starship";
                  sh = "${pkgs.bash}/bin/sh";
                  proFile = cfg.home.file.".profile".source;
                  profile = readFile proFile;
                in nameValuePair user {
                  inherit nixpkgs;
                  useGlobalPkgs = true;
                  useUserPackages = true;
                  editorconfig = {
                    enable = true;
                    bundle = mapAttrs (n: fromTOML) {
                      "*" = ''
                        end_of_line = "lf"
                        insert_final_newline = true
                        indent_style = "space"
                        indent_size = 4
                        charset = "utf-8"
                        trim_trailing_whitespace = true
                        max_line_length = 80
                      '';
                      "{Makefile,makefile,*.mk}" = ''
                        indent_style = "tab"
                        indent_size = "unset"
                        tab_width = 4
                      '';
                      "{*.cmd,*.CMD,*.bat,*.BAT}" = ''
                        end_of_line = "crlf"
                      '';
                      "{*.sh}" = ''
                        end_of_line = "lf"
                      '';
                    };
                  };
                  nix = {
                    checkConfig = true;
                    extraOptions = ''
                      accept-flake-config = true
                      auto-optimise-store = true
                      builders-use-substitutes = true
                      cores = 0
                      extra-experimental-features = nix-command flakes impure-derivations recursive-nix
                      fallback = true
                      flake-registry = https://raw.githubusercontent.com/syvlorg/flake-registry/master/flake-registry.json
                      keep-derivations = true
                      keep-outputs = true
                      max-free = 1073741824
                      min-free = 262144000
                      show-trace = true
                      trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs= nickel.cachix.org-1:ABoCOGpTJbAum7U6c+04VbjvLxG9f0gJP5kYihRRdQs= sylvorg.cachix.org-1:xd1jb7cDkzX+D+Wqt6TemzkJH9u9esXEFu1yaR9p8H8=
                      trusted-substituters = https://cache.nixos.org/ https://nix-community.cachix.org https://nickel.cachix.org https://sylvorg.cachix.org
                      warn-dirty = false
                    '';
                    inherit (bundle) registry;
                  };
                  programs = {
                    assh = {
                      enable = true;
                      hosts = mkMerge [
                        (mapAttrs (n: hostname: {
                          inherit hostname;
                          gateways = flatten [
                            (map (dotcat hostname) [
                              "magic"
                              "tailscale.4"
                              "tailscale.6"
                              "tailapi.4"
                              "tailapi.6"
                              "dns"
                            ])
                            direct
                            "${hostname}.local.hostname"
                            "argus"
                          ];
                        }) { sandshrew = "zsh:1: command not found: tailapi"; })
                        {
                          argus = {
                            hostname = "io.syvl.org";
                            gateways = flatten [
                              (map (dotcat "argus") [
                                "magic"
                                "tailscale.4"
                                "tailscale.6"
                                "tailapi.4"
                                "tailapi.6"
                                "dns"
                              ])
                              "direct"
                              (map (dotcat "argus.wstunnel") [
                                "tls.magic"
                                "tls.tailscale"
                                "tls.tailapi"
                                "tls.dns"
                                "tls.public"
                                "http.magic"
                                "http.tailscale"
                                "http.tailapi"
                                "http.dns"
                                "http.public"
                                "io.magic"
                                "io.tailscale"
                                "io.tailapi"
                                "io.dns"
                                "io.public"
                              ])
                            ];
                          };
                          "*.magic".resolvecommand =
                            ''${sh} -c "echo %h | cut -d '.' -f 1"'';
                          "*.tailscale.*".resolvecommand = ''
                            ${sh} -c "${pkgs.tailscale}/bin/tailscale ip -$(echo %h | rev | cut -d '.' -f 1) $(echo %h | cut -d '.' -f 1)"'';
                          "*.tailapi.*".resolvecommand = ''
                            ${sh} -c "${pkgs.tailapi}/bin/tailapi -rD sylvorg.github --api-key $(pass show keys/api/tailscale/jeet.ray) -d $(echo %h | cut -d '.' -f 1) ip -f$(echo %h | rev | cut -d '.' -f 1)"'';
                          "*.dns".resolvecommand = ''
                            ${sh} -c "echo $(echo %h | cut -d '.' -f 1).syvl.org"'';
                          "*.local.hostname".resolvecommand = ''
                            ${sh} -c "ping -c 1 $(echo %h | cut -d '.' -f 1) | head -1 | awk '{print $3}' | sed 's/[()]//g'"'';
                          "*.local.mac".resolvecommand = ''
                            ${sh} -c "ip neighbor | grep -i $(echo %h | cut -d '.' -f 1) | cut -d ' ' -f 1"'';
                        }
                        (bundle.mapAttrNames (N: V: dotcat "argus" N)
                          (bundle.fold.set [
                            (attrValues (mapAttrs (protocol: port:
                              mapAttrs' (n: v:
                                nameValuePair "wstunnel.${protocol}.${n}" {
                                  proxycommand = v + ''${toString port}"'';
                                }) {
                                  magic = ''
                                    sh -c "wstunnel -L stdio:%h:%p wss://$(echo %h | cut -d '.' -f 1):'';
                                  tailscale = ''
                                    ${sh} -c "wstunnel -L stdio:%h:%p wss://$(tailscale ip -$(echo %h | rev | cut -d '.' -f 1) $(echo %h | cut -d '.' -f 1)):'';
                                  tailapi = ''
                                    ${sh} -c "wstunnel -L stdio:%h:%p wss://$(tailapi -rD sylvorg.github --api-key $(pass show keys/api/tailscale/jeet.ray) -d $(echo %h | cut -d '.' -f 1) ip -f$(echo %h | rev | cut -d '.' -f 1)):'';
                                  dns = ''
                                    ${sh} -c "wstunnel -L stdio:%h:%p wss://io.syvl.org:'';

                                  # TODO: Argus' Public IP
                                  # public = ''${sh} -c "wstunnel -L stdio:%h:%p wss://'';

                                }) {
                                  tls = 443;
                                  http = 80;
                                }))
                            (mapAttrs' (n: proxycommand:
                              nameValuePair (dotcat "iodine" n) {
                                inherit proxycommand;
                              }) {
                                # magic = ''${sh} -c ""'';
                                # tailscale = ''${sh} -c ""'';
                                # tailapi = ''${sh} -c ""'';
                                # dns = ''${sh} -c ""'';
                                # public = ''${sh} -c ""'';
                              })
                          ]))
                      ];
                    };
                    gpg = {
                      enable = true;
                      publicKeys = [{
                        trust = 5;
                        text = ''
                          -----BEGIN PGP PUBLIC KEY BLOCK-----

                          mQINBGJq3x0BEADVv97yaZ2dvNvJhmPWi8MSVpjLa5C1LgOEYU+d2icQgPa+znPq
                          3975Tjh0tN64DpLpRCAOTt8nj+IzohQXrkDJ1VVzcK01HwWgTjYYDGhq7ziUd9yH
                          38wCeiA/uUjXS/l27CZxiQnuUMj0PozLF5YungNtZcUaoXRw2bj/Evkbz3tc+QID
                          FYCv6PPqQDmEZUw/UH6O0CKCtJMvEPbyT9S519Vo1AJ4mCO0HDQduwUiJOmcl3Jl
                          3UrwiIc5jlyjVJr+VNS/DIuXZFXrJE+Xhqj9bHkS7nXgoPGnZVoW9yl7yevoywX4
                          4R7stWEBEkpwpNpBfYvSb4N0zQ4OkbJzK+y6OH2VO7TiqdrtK8c1V8JndRO0Z6Y7
                          eqQ6KgzKoxeW5lCQHBeyDTpk5JkW70Mkkame9KHpNh3ZryMpj5vWanfQ2w9SDusQ
                          ybwJx9rmRtY8oykhpqPkj7orn+MV8r0GjWTVaQ8PuylzjDpj8WG7CUwkO+/wsMpb
                          XwjLFlnHx3zq0q6hPeSk3Oc8e/iaL4QWXgxy6XbMEGcaiamN7+oWtaWS3lmscjHN
                          0/+EYEJc3QYG7nHih3od0KdNxpb8IqM9JMQ7WExXk1n6VnvWNVApqA1Rx2APuIGg
                          TkWlEdNI0ZI0nAZF/idazRZhfqfVotV2BA51T0mGx4VvCm7yNyQnVYKwJQARAQAB
                          tCdKZWV0IFJheSAoamVldC5yYXkpIDxqZWV0LnJheUBzeXZsLm9yZz6JAk4EEwEI
                          ADgWIQTarbDFXR7q9CL8fheMELh2VDUU+AUCYmrfHQIbAwULCQgHAgYVCgkICwIE
                          FgIDAQIeAQIXgAAKCRCMELh2VDUU+MuwD/sF8ubYahmk7Lcqj7JnKwxHWWan7EJa
                          5tQwwGDMRy8w6c/2wHxK2tq4icCisZk5E+quPrp4qFzo8gZVD7ZOM6CsXMBGpr9r
                          eRHXqN+Sw/cc94hlGysjyR9MAW/Mw3p9eMoXvpiF/CV7roN1mASOJi8EIb3b7zNd
                          uVfkjvPkDa6sVoPiQjOQzgCKEjYVvWJT3ffd6YAfNpxtmEACjNXCUOz6SpDyur0k
                          tnwCIrLnhh2m8Rje+R9xoeXrXDvk1S7TK4GuNQrb3BuTYL4ctqRewpjgIwR2HjA7
                          8/JN67TV2dekEiePQ/xShV6OjzuOBAF7pd4djdvxSgIQ49Ye/EaHkK5b7Mgn7UkO
                          LWYiV+A3fP+M/KEyKi9wm2LvKpIBDQbP0tpkU9cBQZ5ZlNvv3FyIB3QfpTR2T79F
                          B3rIWF9gYSDANxYuRNQfyDWb1Cx4UdyME22nqNSD0GW6KbWrABDnrgxg6cbeTdPK
                          v9a3M5dZg2+NMvQt9wiidRYmCUYyqR99xCPMuhS9aqGOxoK89kkVKxmn69yAwmdw
                          FsIPssL8Hu2cJx1LJR3efnoiOWBNkEfGPd6E2jKZicvoSZIp+URulgSCCo6ycVQt
                          iysKuRxH0TOsnINAGl6S6Qo1FEwfbTp1Sle/YxeK83HhSzCvK08nxS9xJlMvwAVj
                          EOPbF8Spb+Hbz7kCDQRiat8dARAAyYTWH54fFr/dvWvC6wpKYEhIoaV+PuSAukOi
                          2FmomwiMPq4WjGL2qc/+/KK/kNL/CUgZxW5jySy5jRo4FWrtTKruOpWyCcW3vSb9
                          TX9vWCP/jc+P/3lU3WJVfRpKuppwpB7Zji7ybfYnEP9ekT2YjgLT3wF9wFtbrkfU
                          YyeI4jOgtL9BRA/3MNCfpdtEowWWF71avzzSg05zI3/2NT6N98oAqAZ1vGEbk836
                          qZGrNm73bXQ5BWjwgN7A1NsR9Lycls8b+zsz5S70jyQmkN3dzpkCLgplDAgk3H7i
                          n5+V3Vs7+6urk/1so17Ev76LhrEhejOr9YlVISHZ5keoPdxW+JZJXtAjeC5DjiOx
                          UcmvKG8hkGE8miv3lVGoeWPCkyDO1pCXveRD4R/a13A3SagdmsJUOc61PQVgfE2l
                          EAOOzDWIs9DD71Bw4UccetAnqracAIxfepx2L/L5JQvLi0ToUdp3PhGfwILnEPaQ
                          RJZG24vXeK3zSsxg8BE1jyToidti0grxFI08Pq/e+G4v9Cawhx1KMQ2w4OMmVzjt
                          v2kFOdo/qYQyJ235ps33iUiyaVP3v24T2CIKCMzWhtZ3BC3RnX7iZKVcjIYvPdo1
                          1ruEHQ1TMHKe8zPWQzMBToBqllEE/RxlrLsqlNKmain/IKC0QkBHPUOS2lfCyzi7
                          1NiupycAEQEAAYkCNgQYAQgAIBYhBNqtsMVdHur0Ivx+F4wQuHZUNRT4BQJiat8d
                          AhsMAAoJEIwQuHZUNRT447QP/1cT14s9kjlIVDkags0ZyiJZachQ1TkRuH0S5lgj
                          j5vFEnII0T1XbmAqbHW4lj8kmwk/vvTfy8LkvcBmxuY+/fi+uTdNVJ9czrFxzqW7
                          KXEeJgXVt4MH3YwPqJVZk2GK7qcqiHxEMP6Pw842rLQ4rN+VtiFXoIQ8hah+hEef
                          Z9WX+NvZ1wH+vo7aB1gEqQfvtxCvmce+G78ef5cKJeGu3abunD94s8EJ5xoXaG4J
                          GStBoK/7QcVVWaJn9liHOstVupiU3isMY0sfjQVOod5s3l/kS8ziI3JF86YOCsM2
                          2PFQaKQHfqskwEqoO/4T40xsq/OjZIH3SdqklKwp9NQe5GmMQFFTEssNaOagxK/q
                          7KT196j+fNyeN+2nJFyJs/kaSYzKYUuTlwpM0Xi/kGNDbSY+E7XDc6TgLBRppz2n
                          bTPU9wd40DZGZWEsad8Bevn7Q6Amv9ZEVhrrhDBOB1eYBtjWTJxGo8BRfREl2WQb
                          nkT/csNLcJRujJoOaRzHEn9RRcTXc5dIWOeZTNZHVqjgQ1LckdUutnkpNy1+MBHQ
                          ktKcDhm1DIqBm5URPGkj3TIgOJfLfGcQRLVO1KSaQG80AogbeaytfL+6qM/CyRGI
                          /jbxRRSQms+WuKZxKmvN3wL0fshSJCtHh33oijjs26VEvUCqsk3YfNThbqf477+w
                          GfJDuQINBGJq6OoBEADO6WDqVwcWWH5DyiktpnFze6GzdBtHXiV/nlyim1J6kXXG
                          fJCfqtYY3TB1nkyoXDoXYe/1ywLf0ezIXqrHH0I9XRqo3ThK/SUJViFDoc2qgtYc
                          B/Bkg3QvZuN2q+1lBrVSyO/C2Pklad4RKWsewOS5BO/BPFFdMu2d4XTpIXxfWztZ
                          TnWs+rp6a1N8TzXEKIzm/QvKXGAIrZ5hHeefgYvTisFfowyLfI4Lm4A4Ofrrskaq
                          8XdlSXaOVNrr5d1FGiv2Hos+/aoojtJX+V+uIlIhMo93broRBTt13u19exKmQF3J
                          A1SKNiJ8RYqU3B3Ri0CJp0bT8aRakxJwp0HhBj8m+mWQY/QYOe3qbWFkG9ccZIsv
                          9Ezg2oQcrLvECDnRWSUN/iICFH4cblAKnubup0A6GF4WyOEXhwl4jJ/GnqfiKdSl
                          8KM4it7LtB6m6P0Vt53FImBzcK4TmsdgEyDq8rPUR0PL10vQRyNKkJz2cqpablq+
                          VX5xucZbPu6w8vG6oRTBrRE+Q9wIoCrySF9qJE/tgImCKEWDWseGLkd4oHYfVhgg
                          oTnMumhUtEN6LMXhusqdyhL5rAHJ3YMSTSrNc2zE+We/it3WFhlc68Daw6Kc/HYw
                          qI8jhQ0OMS3CXNF2RXuxN1FlTZHS06h+2C9nBmbJO1WJ0Xllyi/cn9ISy/bM5QAR
                          AQABiQI2BBgBCAAgFiEE2q2wxV0e6vQi/H4XjBC4dlQ1FPgFAmJq6OoCGyAACgkQ
                          jBC4dlQ1FPgVMRAAu2nfCGDSI6vfXy9lvX9IUffTmV/aDidnamYYqLTLBRVcMe5T
                          SKOVqk3I4e4HOPMAmm+WmIJLFPqx3GDnmx6hvG0Edwcx+FkfpidrC8qU5aWhyRAS
                          MXMdfPs4mgRzQezvU+i4QqeS7k37OcPOobwqzLIjkq+0/ELFiaoCBLKscAE+juqq
                          EmrBh3lFKHsWUF3YQSZ8PqjDGywFRO9dm+zc+ZE7AAqr7niGipjxNCoPG7bOX+Rd
                          uv1Ko845+5idlDkpi4bsQbFpOLlEIkQcbnQHGPVlRRukApd9Wo+uUJxmUHNcL6ys
                          qkXs7535ZPeCLdRQ2IS8OsoaeUnkJJl0XC6Old52bOkUtE+nUkFv6eyg+2KXAWKA
                          4pDZD9n2YNc+MQnJTt31yotBNUEZoCBPjT9a3/2vsBRakyd992jxDJC300DwGJrE
                          KX/7WGPbVeSnC29yBaZgsiXz9v6S3UF1RMOe7PkKPPY4EQiijNdmzchZRbVMI//a
                          EyyJlCOTMkPId7Dctes5Yp6KROzhPFyTyOEeYM5tfm46cRGNkETkBgw+/5tMLA5Z
                          qtvXo3WfQFAUHv6DI90XKhaB3+FeXsosBW5LmWJuy+tLnypiJR7nY0RHmYVI2ZXn
                          MibZQg+ZAwmehn7ytlU3FiHPx4fYZd6gSsIUPZJ1twx4/5sW6nUv+iK6V0S5Ag0E
                          YmrpVgEQAL/PWZUr+WRg7LHYK1Uz1x8Wa0rkNIjU3fGvYBcElg63hrGnCPCp4N4J
                          nYwWZuNn4FJ1qDBm7merwUz5jCY2MRojEEHyMpluOy48Z+zVixOyDz6acD5LQzQE
                          2pLx72Gry+dxpOqbC6iLytY465SYMOYHT1tt+fbBUuo+eLsMP+0y6TIEYldHffKB
                          NZQDTf0FvWrTqn599zpG/3cHmAJ0LO3Hr1lG0d6bnXyYzoKyJUwu/kxhZ6Ypy0bZ
                          m/TyzG80K6vWmaEmi6avyMx4EqfMxFxw0hkUiwtXw3HyStlUbGSL+/cnhWv1wRFZ
                          TSj8EANHw3awDSoJK5+TIFWzbkyJ9fVRabX6X59eZ96zlhXEggX7Rct2aCLpnlcw
                          vYjySxZ5lReulTXsZUsHMjHzCkaweoRtBqKVYGmeYIqId3CbMfLx87X2z/++T9jP
                          3hIKaJg0VHSDCSEtsunWazPnlNAOMkk3GKUUQ+/f8nlQcm0G0iJr0Cf+M2U6fU2u
                          mmc9Mwbt6wj3xslsWjS5oPIdHHN/Iv3qamGCOtuSCEaGC25uc4d3WEy17RhhwTa9
                          AWPc5pSOoL6tlKXy0sQnyFCegkwehvoieLrIUgjWg/0cdUmziYxhXECXOwWPyhCm
                          i8Om2l8yzrv9JGRDhyKyhMQQf8JvbKcuBZagqq756yu29AHSQqJ9ABEBAAGJBGwE
                          GAEIACAWIQTarbDFXR7q9CL8fheMELh2VDUU+AUCYmrpVgIbAgJACRCMELh2VDUU
                          +MF0IAQZAQgAHRYhBEdm0dTZAEMb9fn06yCVykzJwQUsBQJiaulWAAoJECCVykzJ
                          wQUswQUQAIa5SxgY0cHz1aP/CUDF41SQuq1+nAqUmwXndNJ42iTAH/vGOVNBiXEH
                          Q55CQO8mOHw0F0omhEg5cDdhh+YyP2pSHxyWqTGwY+RqVdLIkl4pAopOaAyGRoyD
                          N333wJoRw+lVnqACPDe5pdXCWWrllfuiZvQfsKs4t7qGYIAn+TY4Uw+lHbMknYb/
                          uDEPg03uSintuyu2l/yIGArMa0Ij3sh0jyfiFTaiylPafWBM/NfLnoCVSTbMURp5
                          7nGM3BVp0yKYMhey20KrY+GFoDCeVhFn6E0cqh5NRmL853nG59JrT5nHBl2AaVIr
                          +Vgwwsv1ltbCA2Jrb7V84Y7TjG90LHgRMVVsadzX1o4PMN2AoJ2plTj6tFTCDZDk
                          KBm8DmaV6Rygt0rZhAztPW7jg/X5lkx2biiNJLFyPzYBxfGnd6/JCzhakHYETAmH
                          srl0prUHrrcAH0pd7IZPandCE05bM9hOb7d4VtA7IT00TWeKn2M3Tf3QmE0hbcxb
                          k/c+1ZRIxrx8HyQDkGFRVu4GwjTmSU5uRGu0Q9FvclRsxaAm7TMVUAZ/Q3ZLNta6
                          b8TwGdbXHQOcG3DEO/awY0DhkWhuQrgnk7d+MtoV3xONbGb9H7kQtEadubd1EcNU
                          M6I0oTdwvoHg/mcos9UWzTjY3kLa5bwNMeAKHcQfukio+giKuwFp6RkQAMIM3Hoh
                          WQZPQqpU7/GsEt++onMP3G4RUKevUKH5meecmcgeu9NC9A+vT2GrrUVFh0juLIBE
                          pT0FEuiduMj1zp+ByH1ywl0EnrtUPuJNjtqZLlWjfyVCz318Fo9LTB0yWUGQUU+L
                          UM6qXWd1W2hGk4x4SNi2C5gnJ04fyPkP0dyxKbnQqTYQlltLC5pbgVRxq/0CmtCC
                          uIMShTQ9CaX0w3VvMbra6fJgkZuRk8SOwFmA3YZ+a+CaYvo6PL9FQjxPIJ4PuBKL
                          zUdJw4IOIHJi+n9KuwINwhvlJqvxFMY5vjLieI8moNezhVcGEwbGa/XT0mvZG6xP
                          t0Wp/LtH8vM4pSxv+ftmmpiQhm0vL+17tlX3NDTp3HIySF5J1q5nEn3JpjmKIsGS
                          FeHd85EM5NcVGyFSF11Q3XJciLz08H+xcJw1C7C/vV9dNX/icJRtW45+4lkBjBan
                          7EtMyTfkQ9wahN8O7T32OoWrhl6QZe46CsKc9FEK2SdlAKyy0bG9MPZJ1kHxN/1c
                          nkHgXg25cecCCN8Is/mEFQ650057Rqyz3zNtH+EVZwFNJGVgFMzrLQ0Q6QY+9A7J
                          UFeHy12N2RGsGJpoJFoF1f0TrPinYU7TivvUJeWulGWKGr3CZHY//3NAji74iUsr
                          REgsM22cpS836D7wEX0rtEYHPj9YAkfj53yI
                          =LhpQ
                          -----END PGP PUBLIC KEY BLOCK-----
                        '';
                      }];
                    };
                    ion = {
                      enable = true;
                      inherit (cfg) shellAliases;
                    };
                    xonsh = {
                      inherit (home) sessionPath;
                      imports = [ "sys" "os" ];
                      xontribs = [
                        # "coreutils"
                        # "docker_tabcomplete"
                        # "schedule"
                        # "ssh_agent"
                        # "vox_tabcomplete"

                        "abbrevs"
                        "autoxsh"
                        "bashisms"
                        "pipeliner"
                        "readable-traceback"
                        "sh"
                        "vox"
                        "whole_word_jumping"
                      ];
                      fromImports.pathlib = "Path";
                      sessionVariablesAppend.bash_completions =
                        toList inputs.bash-completion;
                      sessionVariables = mkMerge [
                        sessionVariables
                        (genAttrs [
                          "auto_cd"
                          "auto_pushd"
                          "auto_suggest_in_completions"
                          "auto_suggest"
                          "completion_in_thread"
                          "completions_confirm"
                          "dotglob"
                          "fuzzy_path_completion"
                          "mouse_support"
                          "pretty_print_results"
                          "update_completions_on_keypress"
                          "update_os_environ"
                          "vi_mode"
                          "xonsh_autopair"
                          "xonsh_cache_everything"
                          "xonsh_show_traceback"
                          "xonsh_store_stdout"
                        ] (n: true))
                        {
                          histcontrol = f:
                            ''{ "ignoreboth", "erasedups", "ignoreerr" }'';
                          prompt_toolkit_color_depth = "DEPTH_24_BIT";
                          shell_type = "prompt_toolkit";
                          xonsh_debug = false;
                          xonsh_history_backend = "sqlite";
                        }
                      ];
                      shellAliases = mkMerge [
                        shellAliases

                        # TODO: Rename and document these, then put these in a proper set:
                        (genAttrs (a: f: "_" + a) [ "cdf" "cdi" "cdr" ])

                        {
                          mdg = f: "mkdir_and_go";
                          rc = f: "recent_commands";
                          run = f: "_run";
                          Run = f: "_Run";
                          da = f: "direnv_allow";
                          git = f:
                            "lambda args, stdin=None: $(${pkgs.hub}/bin/hub @(args))";
                          la = f:
                            "lambda args, stdin=None: $(${cfg.programs.exa.package}/bin/exa -la --octal-permissions @(args))";
                          md = f:
                            "lambda args, stdin=None: $(mkdir -p @(args))";
                          mosh = f:
                            "lambda args, stdin=None: $(${pkgs.mosh}/bin/mosh --experimental-remote-ip=remote @(args))";
                          ve = f:
                            "lambda args, stdin=None: $(vox enter @(args)) if args else $(vox exit)";
                          e = f:
                            "lambda args, stdin=None: $(${fasd} -fe '${emacsclient} -c' @(args))";
                          o = f:
                            "lambda args, stdin=None: $(${fasd} -ae xdg-open @(args))";
                          s = "source ${cfg.home.file.".xonshrc".source}";
                        }
                      ];
                      functions = "";
                    };
                    elvish = {
                      inherit (home) sessionPath sessionVariables shellAliases;
                      enable = true;
                      evaluations = [
                        "(${zoxide} init elvish | slurp)"
                        "(${starship} init elvish)"
                      ];
                    };
                    zoxide = {
                      enable = true;
                      enableBashIntegration = true;
                      enableElvishIntegration = true;
                      enableFishIntegration = true;
                      enableXonshIntegration = true;
                      enableZshIntegration = true;
                    };
                    direnv = {
                      enable = true;
                      enableBashIntegration = true;
                      enableFishIntegration = true;
                      enableXonshIntegration = true;
                      enableZshIntegration = true;
                    };
                    fzf = {
                      enable = true;
                      enableBashIntegration = true;
                      enableFishIntegration = true;
                      enableZshIntegration = true;
                      defaultCommand = "${pkgs.fd}/bin/fd --type f";
                      tmux.enableShellIntegration = true;
                    };
                    exa = {
                      enable = true;
                      enableAliases = true;
                    };
                    bat.enable = true;
                    git = {
                      enable = true;
                      package = pkgs.hub;
                      userEmail = if ("titaniumfiles@outlook.com"
                        == "titaniumfiles@outlook.com") then
                        "jeet.ray@syvl.org"
                      else
                        "titaniumfiles@outlook.com";
                      userName = "Jeet Ray";
                      signing = {
                        gpgPath = "${cfg.programs.gpg.package}/bin/gpg";
                        key = "DAADB0C55D1EEAF422FC7E178C10B876543514F8";
                        signByDefault = true;
                      };
                      extraConfig = {
                        core = {
                          excludesfile = cfg.xdg.configFile."git/ignore".source;
                          attributesfile =
                            cfg.xdg.configFile."git/attributes".source;
                          fileMode = false;
                          hooksPath = "${homeDirectory}/.config/git/hooks";
                        };
                        protocol = {
                          keybase.allow = "always";
                          restic.allow = "always";
                        };
                        pull.rebase = false;
                        filter.git = {
                          clean = "${pkgs.bat}/bin/bat";
                          smudge = "${pkgs.bat}/bin/bat";
                        };
                        diff.git.command = "git diff";
                      };
                      lfs.enable = true;
                      ignores = splitString "\n" "\n";
                      attributes = splitString "\n" ''
                        ####################
                        # Git Line Endings #
                        ####################

                        # Set default behaviour to automatically normalize line endings.
                        * text=auto

                        # Force batch scripts to always use CRLF line endings so that if a repo is accessed
                        # in Windows via a file share from Linux, the scripts will work.
                        *.{cmd,[cC][mM][dD]} text eol=crlf
                        *.{bat,[bB][aA][tT]} text eol=crlf

                        # Force bash scripts to always use LF line endings so that if a repo is accessed
                        # in Unix via a file share from Windows, the scripts will work.
                        *.sh text eol=lf
                        *.3dm filter=lfs diff=lfs merge=lfs -text
                        *.3ds filter=lfs diff=lfs merge=lfs -text
                        *.3g2 filter=lfs diff=lfs merge=lfs -text
                        *.3gp filter=lfs diff=lfs merge=lfs -text
                        *.7z filter=lfs diff=lfs merge=lfs -text
                        *.a filter=lfs diff=lfs merge=lfs -text
                        *.aac filter=lfs diff=lfs merge=lfs -text
                        *.aax filter=lfs diff=lfs merge=lfs -text
                        *.adp filter=lfs diff=lfs merge=lfs -text
                        *.ai filter=lfs diff=lfs merge=lfs -text
                        *.aif filter=lfs diff=lfs merge=lfs -text
                        *.aiff filter=lfs diff=lfs merge=lfs -text
                        *.alz filter=lfs diff=lfs merge=lfs -text
                        *.ape filter=lfs diff=lfs merge=lfs -text
                        *.apk filter=lfs diff=lfs merge=lfs -text
                        *.appimage filter=lfs diff=lfs merge=lfs -text
                        *.ar filter=lfs diff=lfs merge=lfs -text
                        *.arj filter=lfs diff=lfs merge=lfs -text
                        *.asf filter=lfs diff=lfs merge=lfs -text
                        *.au filter=lfs diff=lfs merge=lfs -text
                        *.avi filter=lfs diff=lfs merge=lfs -text
                        *.baml filter=lfs diff=lfs merge=lfs -text
                        *.bh filter=lfs diff=lfs merge=lfs -text
                        *.bin filter=lfs diff=lfs merge=lfs -text
                        *.bk filter=lfs diff=lfs merge=lfs -text
                        *.bmp filter=lfs diff=lfs merge=lfs -text
                        *.br filter=lfs diff=lfs merge=lfs -text
                        *.btif filter=lfs diff=lfs merge=lfs -text
                        *.bz2 filter=lfs diff=lfs merge=lfs -text
                        *.bzip2 filter=lfs diff=lfs merge=lfs -text
                        *.cab filter=lfs diff=lfs merge=lfs -text
                        *.caf filter=lfs diff=lfs merge=lfs -text
                        *.cgm filter=lfs diff=lfs merge=lfs -text
                        *.class filter=lfs diff=lfs merge=lfs -text
                        *.cmx filter=lfs diff=lfs merge=lfs -text
                        *.cpio filter=lfs diff=lfs merge=lfs -text
                        *.cr2 filter=lfs diff=lfs merge=lfs -text
                        *.cur filter=lfs diff=lfs merge=lfs -text
                        *.dat filter=lfs diff=lfs merge=lfs -text
                        *.dcm filter=lfs diff=lfs merge=lfs -text
                        *.deb filter=lfs diff=lfs merge=lfs -text
                        *.dex filter=lfs diff=lfs merge=lfs -text
                        *.djvu filter=lfs diff=lfs merge=lfs -text
                        *.dll filter=lfs diff=lfs merge=lfs -text
                        *.dmg filter=lfs diff=lfs merge=lfs -text
                        *.dng filter=lfs diff=lfs merge=lfs -text
                        *.doc filter=lfs diff=lfs merge=lfs -text
                        *.docm filter=lfs diff=lfs merge=lfs -text
                        *.docx filter=lfs diff=lfs merge=lfs -text
                        *.dot filter=lfs diff=lfs merge=lfs -text
                        *.dotm filter=lfs diff=lfs merge=lfs -text
                        *.dra filter=lfs diff=lfs merge=lfs -text
                        *.DS_Store filter=lfs diff=lfs merge=lfs -text
                        *.dsk filter=lfs diff=lfs merge=lfs -text
                        *.dts filter=lfs diff=lfs merge=lfs -text
                        *.dtshd filter=lfs diff=lfs merge=lfs -text
                        *.dvb filter=lfs diff=lfs merge=lfs -text
                        *.dwg filter=lfs diff=lfs merge=lfs -text
                        *.dxf filter=lfs diff=lfs merge=lfs -text
                        *.ecelp4800 filter=lfs diff=lfs merge=lfs -text
                        *.ecelp7470 filter=lfs diff=lfs merge=lfs -text
                        *.ecelp9600 filter=lfs diff=lfs merge=lfs -text
                        *.egg filter=lfs diff=lfs merge=lfs -text
                        *.eol filter=lfs diff=lfs merge=lfs -text
                        *.eot filter=lfs diff=lfs merge=lfs -text
                        *.epub filter=lfs diff=lfs merge=lfs -text
                        *.exe filter=lfs diff=lfs merge=lfs -text
                        *.f4v filter=lfs diff=lfs merge=lfs -text
                        *.fbs filter=lfs diff=lfs merge=lfs -text
                        *.fh filter=lfs diff=lfs merge=lfs -text
                        *.fla filter=lfs diff=lfs merge=lfs -text
                        *.flac filter=lfs diff=lfs merge=lfs -text
                        *.flatpak filter=lfs diff=lfs merge=lfs -text
                        *.fli filter=lfs diff=lfs merge=lfs -text
                        *.flv filter=lfs diff=lfs merge=lfs -text
                        *.fpx filter=lfs diff=lfs merge=lfs -text
                        *.fst filter=lfs diff=lfs merge=lfs -text
                        *.fvt filter=lfs diff=lfs merge=lfs -text
                        *.g3 filter=lfs diff=lfs merge=lfs -text
                        *.gh filter=lfs diff=lfs merge=lfs -text
                        *.gif filter=lfs diff=lfs merge=lfs -text
                        *.graffle filter=lfs diff=lfs merge=lfs -text
                        *.gz filter=lfs diff=lfs merge=lfs -text
                        *.gzip filter=lfs diff=lfs merge=lfs -text
                        *.h261 filter=lfs diff=lfs merge=lfs -text
                        *.h263 filter=lfs diff=lfs merge=lfs -text
                        *.h264 filter=lfs diff=lfs merge=lfs -text
                        *.icns filter=lfs diff=lfs merge=lfs -text
                        *.ico filter=lfs diff=lfs merge=lfs -text
                        *.ief filter=lfs diff=lfs merge=lfs -text
                        *.img filter=lfs diff=lfs merge=lfs -text
                        *.ipa filter=lfs diff=lfs merge=lfs -text
                        *.iso filter=lfs diff=lfs merge=lfs -text
                        *.jar filter=lfs diff=lfs merge=lfs -text
                        *.jpeg filter=lfs diff=lfs merge=lfs -text
                        *.jpg filter=lfs diff=lfs merge=lfs -text
                        *.jpgv filter=lfs diff=lfs merge=lfs -text
                        *.jpm filter=lfs diff=lfs merge=lfs -text
                        *.jxr filter=lfs diff=lfs merge=lfs -text
                        *.key filter=lfs diff=lfs merge=lfs -text
                        *.ktx filter=lfs diff=lfs merge=lfs -text
                        *.lha filter=lfs diff=lfs merge=lfs -text
                        *.lib filter=lfs diff=lfs merge=lfs -text
                        *.lvp filter=lfs diff=lfs merge=lfs -text
                        *.lz filter=lfs diff=lfs merge=lfs -text
                        *.lzh filter=lfs diff=lfs merge=lfs -text
                        *.lzma filter=lfs diff=lfs merge=lfs -text
                        *.lzo filter=lfs diff=lfs merge=lfs -text
                        *.m3u filter=lfs diff=lfs merge=lfs -text
                        *.m4a filter=lfs diff=lfs merge=lfs -text
                        *.m4v filter=lfs diff=lfs merge=lfs -text
                        *.mar filter=lfs diff=lfs merge=lfs -text
                        *.mdi filter=lfs diff=lfs merge=lfs -text
                        *.mht filter=lfs diff=lfs merge=lfs -text
                        *.mid filter=lfs diff=lfs merge=lfs -text
                        *.midi filter=lfs diff=lfs merge=lfs -text
                        *.mj2 filter=lfs diff=lfs merge=lfs -text
                        *.mka filter=lfs diff=lfs merge=lfs -text
                        *.mkv filter=lfs diff=lfs merge=lfs -text
                        *.mmr filter=lfs diff=lfs merge=lfs -text
                        *.mng filter=lfs diff=lfs merge=lfs -text
                        *.mobi filter=lfs diff=lfs merge=lfs -text
                        *.mov filter=lfs diff=lfs merge=lfs -text
                        *.movie filter=lfs diff=lfs merge=lfs -text
                        *.mp3 filter=lfs diff=lfs merge=lfs -text
                        *.mp4 filter=lfs diff=lfs merge=lfs -text
                        *.mp4a filter=lfs diff=lfs merge=lfs -text
                        *.mpeg filter=lfs diff=lfs merge=lfs -text
                        *.mpg filter=lfs diff=lfs merge=lfs -text
                        *.mpga filter=lfs diff=lfs merge=lfs -text
                        *.mxu filter=lfs diff=lfs merge=lfs -text
                        *.nef filter=lfs diff=lfs merge=lfs -text
                        *.npx filter=lfs diff=lfs merge=lfs -text
                        *.numbers filter=lfs diff=lfs merge=lfs -text
                        *.nupkg filter=lfs diff=lfs merge=lfs -text
                        *.o filter=lfs diff=lfs merge=lfs -text
                        *.odp filter=lfs diff=lfs merge=lfs -text
                        *.ods filter=lfs diff=lfs merge=lfs -text
                        *.odt filter=lfs diff=lfs merge=lfs -text
                        *.oga filter=lfs diff=lfs merge=lfs -text
                        *.ogg filter=lfs diff=lfs merge=lfs -text
                        *.ogv filter=lfs diff=lfs merge=lfs -text
                        *.otf filter=lfs diff=lfs merge=lfs -text
                        *.ott filter=lfs diff=lfs merge=lfs -text
                        *.pages filter=lfs diff=lfs merge=lfs -text
                        *.pbm filter=lfs diff=lfs merge=lfs -text
                        *.pcx filter=lfs diff=lfs merge=lfs -text
                        *.pdb filter=lfs diff=lfs merge=lfs -text
                        *.pdf filter=lfs diff=lfs merge=lfs -text
                        *.pea filter=lfs diff=lfs merge=lfs -text
                        *.pgm filter=lfs diff=lfs merge=lfs -text
                        *.pic filter=lfs diff=lfs merge=lfs -text
                        *.png filter=lfs diff=lfs merge=lfs -text
                        *.pnm filter=lfs diff=lfs merge=lfs -text
                        *.pot filter=lfs diff=lfs merge=lfs -text
                        *.potm filter=lfs diff=lfs merge=lfs -text
                        *.potx filter=lfs diff=lfs merge=lfs -text
                        *.ppa filter=lfs diff=lfs merge=lfs -text
                        *.ppam filter=lfs diff=lfs merge=lfs -text
                        *.ppm filter=lfs diff=lfs merge=lfs -text
                        *.pps filter=lfs diff=lfs merge=lfs -text
                        *.ppsm filter=lfs diff=lfs merge=lfs -text
                        *.ppsx filter=lfs diff=lfs merge=lfs -text
                        *.ppt filter=lfs diff=lfs merge=lfs -text
                        *.pptm filter=lfs diff=lfs merge=lfs -text
                        *.pptx filter=lfs diff=lfs merge=lfs -text
                        *.psd filter=lfs diff=lfs merge=lfs -text
                        *.pya filter=lfs diff=lfs merge=lfs -text
                        *.pyc filter=lfs diff=lfs merge=lfs -text
                        *.pyo filter=lfs diff=lfs merge=lfs -text
                        *.pyv filter=lfs diff=lfs merge=lfs -text
                        *.qt filter=lfs diff=lfs merge=lfs -text
                        *.rar filter=lfs diff=lfs merge=lfs -text
                        *.ras filter=lfs diff=lfs merge=lfs -text
                        *.raw filter=lfs diff=lfs merge=lfs -text
                        *.resources filter=lfs diff=lfs merge=lfs -text
                        *.rgb filter=lfs diff=lfs merge=lfs -text
                        *.rip filter=lfs diff=lfs merge=lfs -text
                        *.rlc filter=lfs diff=lfs merge=lfs -text
                        *.rmf filter=lfs diff=lfs merge=lfs -text
                        *.rmvb filter=lfs diff=lfs merge=lfs -text
                        *.rpm filter=lfs diff=lfs merge=lfs -text
                        *.rtf filter=lfs diff=lfs merge=lfs -text
                        *.rz filter=lfs diff=lfs merge=lfs -text
                        *.s3m filter=lfs diff=lfs merge=lfs -text
                        *.s7z filter=lfs diff=lfs merge=lfs -text
                        *.scpt filter=lfs diff=lfs merge=lfs -text
                        *.sgi filter=lfs diff=lfs merge=lfs -text
                        *.shar filter=lfs diff=lfs merge=lfs -text
                        *.snap filter=lfs diff=lfs merge=lfs -text
                        *.sil filter=lfs diff=lfs merge=lfs -text
                        *.sketch filter=lfs diff=lfs merge=lfs -text
                        *.slk filter=lfs diff=lfs merge=lfs -text
                        *.smv filter=lfs diff=lfs merge=lfs -text
                        *.snk filter=lfs diff=lfs merge=lfs -text
                        *.so filter=lfs diff=lfs merge=lfs -text
                        *.stl filter=lfs diff=lfs merge=lfs -text
                        *.suo filter=lfs diff=lfs merge=lfs -text
                        *.sub filter=lfs diff=lfs merge=lfs -text
                        *.swf filter=lfs diff=lfs merge=lfs -text
                        *.tar filter=lfs diff=lfs merge=lfs -text
                        *.tbz filter=lfs diff=lfs merge=lfs -text
                        *.tbz2 filter=lfs diff=lfs merge=lfs -text
                        *.tga filter=lfs diff=lfs merge=lfs -text
                        *.tgz filter=lfs diff=lfs merge=lfs -text
                        *.thmx filter=lfs diff=lfs merge=lfs -text
                        *.tif filter=lfs diff=lfs merge=lfs -text
                        *.tiff filter=lfs diff=lfs merge=lfs -text
                        *.tlz filter=lfs diff=lfs merge=lfs -text
                        *.ttc filter=lfs diff=lfs merge=lfs -text
                        *.ttf filter=lfs diff=lfs merge=lfs -text
                        *.txz filter=lfs diff=lfs merge=lfs -text
                        *.udf filter=lfs diff=lfs merge=lfs -text
                        *.uvh filter=lfs diff=lfs merge=lfs -text
                        *.uvi filter=lfs diff=lfs merge=lfs -text
                        *.uvm filter=lfs diff=lfs merge=lfs -text
                        *.uvp filter=lfs diff=lfs merge=lfs -text
                        *.uvs filter=lfs diff=lfs merge=lfs -text
                        *.uvu filter=lfs diff=lfs merge=lfs -text
                        *.viv filter=lfs diff=lfs merge=lfs -text
                        *.vob filter=lfs diff=lfs merge=lfs -text
                        *.war filter=lfs diff=lfs merge=lfs -text
                        *.wav filter=lfs diff=lfs merge=lfs -text
                        *.wax filter=lfs diff=lfs merge=lfs -text
                        *.wbmp filter=lfs diff=lfs merge=lfs -text
                        *.wdp filter=lfs diff=lfs merge=lfs -text
                        *.weba filter=lfs diff=lfs merge=lfs -text
                        *.webm filter=lfs diff=lfs merge=lfs -text
                        *.webp filter=lfs diff=lfs merge=lfs -text
                        *.whl filter=lfs diff=lfs merge=lfs -text
                        *.wim filter=lfs diff=lfs merge=lfs -text
                        *.wm filter=lfs diff=lfs merge=lfs -text
                        *.wma filter=lfs diff=lfs merge=lfs -text
                        *.wmv filter=lfs diff=lfs merge=lfs -text
                        *.wmx filter=lfs diff=lfs merge=lfs -text
                        *.woff filter=lfs diff=lfs merge=lfs -text
                        *.woff2 filter=lfs diff=lfs merge=lfs -text
                        *.wrm filter=lfs diff=lfs merge=lfs -text
                        *.wvx filter=lfs diff=lfs merge=lfs -text
                        *.xbm filter=lfs diff=lfs merge=lfs -text
                        *.xif filter=lfs diff=lfs merge=lfs -text
                        *.xla filter=lfs diff=lfs merge=lfs -text
                        *.xlam filter=lfs diff=lfs merge=lfs -text
                        *.xls filter=lfs diff=lfs merge=lfs -text
                        *.xlsb filter=lfs diff=lfs merge=lfs -text
                        *.xlsm filter=lfs diff=lfs merge=lfs -text
                        *.xlsx filter=lfs diff=lfs merge=lfs -text
                        *.xlt filter=lfs diff=lfs merge=lfs -text
                        *.xltm filter=lfs diff=lfs merge=lfs -text
                        *.xltx filter=lfs diff=lfs merge=lfs -text
                        *.xm filter=lfs diff=lfs merge=lfs -text
                        *.xmind filter=lfs diff=lfs merge=lfs -text
                        *.xpi filter=lfs diff=lfs merge=lfs -text
                        *.xpm filter=lfs diff=lfs merge=lfs -text
                        *.xwd filter=lfs diff=lfs merge=lfs -text
                        *.xz filter=lfs diff=lfs merge=lfs -text
                        *.z filter=lfs diff=lfs merge=lfs -text
                        *.zip filter=lfs diff=lfs merge=lfs -text
                        *.zipx filter=lfs diff=lfs merge=lfs -text
                        black.aiern.org filter=git-crypt diff=git-crypt

                        etc/nixos/secrets/**/* filter=git-crypt diff=git-crypt

                        .termux/font.ttf filter=git-crypt diff=git-crypt

                        .ssh/**/* filter=git-crypt diff=git-crypt
                        .ssh/**/*.pub* -filter -diff
                        .ssh/**/assh.yml -filter -diff

                        .gnupgk/**/* filter=git-crypt diff=git-crypt
                        .gnupgk/**/*.pub* -filter -diff

                        .fonts/cartographcf-* filter=git-crypt diff=git-crypt

                        .config/borg/keys/**/* filter=git-crypt diff=git-crypt
                        .config/rclone/rclone.conf filter=git-crypt diff=git-crypt

                        **/.password.tomb.key filter=git-crypt diff=git-crypt
                        etc/caddy/**/* filter=git-crypt diff=git-crypt
                        etc/tailscale/**/* filter=git-crypt diff=git-crypt
                      '';
                      aliases = let
                        setup = ''
                          #! /usr/bin/env nix-shell
                          #! nix-shell -E "(import ${inputs.user}).devShells.${builtins.currentSystem}.git-shell"
                          #! nix-shell -i sh
                          set -eo pipefail
                          root=$(git rev-parse --show-toplevel)
                        '';
                        dekey = pkgs.writeShellScript "dekey" ''
                          ${setup}
                          for gpgID in $@; do
                              rm "$root/.git-crypt/keys/default/0/$(gpg --fingerprint $gpgID | sed -n 2p | tr -d ' ').gpg"
                          done
                        '';
                        rekey = pkgs.writeShellScript "rekey" ''
                          ${setup}
                          tmpf=$(mktemp)
                          tmpd=$(mktemp -d)
                          tmpCrypt=$(mktemp -d)
                          trap "rm -rf $tmpf $tmpd $tmpCrypt" 0 2 3 15
                          if [ -f "$root/.git" ]; then
                              gitCrypt=$(realpath $(cat "$root/.git" | sed 's/.*: //'))
                          else
                              gitCrypt="$root/.git/git-crypt"
                          fi
                          git crypt unlock
                          git encrypted > $tmpf
                          rsync -avvczz --files-from $tmpf "$root" $tmpd
                          rsync -avvczz "$root/.git-crypt/" $tmpCrypt/
                          rm -rf "$gitCrypt" "$root/.git-crypt"
                          git -C "$root" filter-repo --paths-from-file $tmpf --invert-paths --force
                          rsync -avvczz $tmpd/ "$root"/
                          git crypt init
                          for key in $(ls $tmpCrypt/keys/default/0/*gpg); do
                              git crypt add-gpg-user $(basename $key .gpg)
                          done
                          [ -n "$1" ] && git crypt add-gpg-user $@ || :
                        '';
                      in {
                        cnm = "commit --allow-empty-message -am ''";
                        chRun = "!chmod +x $1 &&";
                        user = ''!git -C "${homeDirectory}/user"'';
                        commit-user = "user cnm";
                        push-user = "user push";
                        super-user = "!git commit-user && git push-user";
                        bundle =
                          ''!git -C "$(git rev-parse --show-toplevel)/bundle"'';
                        bundle-up = ''
                          !git bundle pull origin main && git add "$(git rev-parse --show-toplevel)/bundle" && git commit -m "Updated bundle repo"'';
                        usp = "!git us && git push";
                        sub = "submodule update --init --recursive --remote";
                        submerge = "sub --merge";
                        crossmerge =
                          "!GIT_DISCOVERY_ACROSS_FILESYSTEM=1 git submerge";
                        damerge = "!GIT_SSL_NO_VERIFY=1 git crossmerge";
                        subinit = "sub --force";
                        crossinit =
                          "!GIT_DISCOVERY_ACROSS_FILESYSTEM=1 git subinit";
                        daminit = "!GIT_SSL_NO_VERIFY=1 git crossinit";
                        encrypted = "!git crypt status -e | sed 's/.*: //'";
                        decrypted = "!git crypt status -u | sed 's/.*: //'";
                        dekey = "!${dekey}";
                        rekey = "!${rekey}";
                      };
                    };
                    neovim.enable = true;
                    vim.enable = true;
                    spacevim = {
                      enable = true;
                      extraInit = ''
                        #=======================================================================#
                        # dark_powered.toml --- dark powered configuration example for SpaceVim #
                        # Copyright (c) 2016-2020 Wang Shidong & Contributors                   #
                        # Author: Wang Shidong < wsdjeg at 163.com >                            #
                        # URL: https://spacevim.org                                             #
                        # License: GPLv3                                                        #
                        #=======================================================================#
                      '';
                      layers = [
                        {
                          name = "autocomplete";
                          auto_completion_return_key_behavior = "complete";
                          auto_completion_tab_key_behavior = "smart";
                        }
                        {
                          name = "shell";
                          default_position = "top";
                          default_height = 30;
                        }
                        { name = "lang#nix"; }
                      ];
                      plugins = [
                        "tssm/fairyfloss.vim"
                        # "co1ncidence/mountaineer.vim"
                        # "co1ncidence/gunmetal.vim"
                        # "megantiu/true.vim"
                        "arp242/auto_mkdir2.vim"
                        "tpope/vim-eunuch"
                        # "johannesthyssen/vim-signit"
                        "jupyter-vim/jupyter-vim"
                        "haya14busa/dein-command.vim"
                        "andrep/vimacs"
                        "shougo/denite.nvim"
                        "shougo/defx.nvim"
                        "shougo/deoplete.nvim"
                      ];
                      options = {
                        colorscheme = "gruvbox";
                        colorscheme_bg = "dark";
                        enable_guicolors = true;
                        statusline_separator = "arrow";
                        statusline_iseparator = "arrow";
                        buffer_index_type = 4;
                        enable_tabline_filetype_icon = true;
                        enable_statusline_mode = false;
                        bootstrap_before = "vimrc#before";
                        bootstrap_after = "vimrc#after";
                        escape_key_binding = "";
                      };
                      vimAutoloads.vimrc = ''
                        function! vimrc#before() abort
                        endfunction

                        function! vimrc#after() abort
                            syntax on
                            filetype plugin indent on

                            " enable 24bit true color
                            if (has("termguicolors"))
                            set termguicolors
                            endif

                            " enable the theme
                            syntax enable
                            colorscheme fairyfloss

                            let g:true_airline = 1
                            let g:airline_theme='true'

                            " for kitty
                            let &t_ut=""

                            " disables border on left side
                            set foldcolumn=0

                            " Spaces & Tabs
                            set tabstop=4       " number of visual spaces per TAB
                            set softtabstop=4   " number of spaces in tab when editing
                            set shiftwidth=4    " number of spaces to use for autoindent
                            set expandtab       " tabs are space
                            set autoindent
                            set copyindent      " copy indent from the previous line

                            " set number relativenumber

                            " Insert Toggle
                            imap ;; <ESC>
                            map ;; i <BACKSPACE>

                            " Change two spaces to four
                            map \\ :set ts=2 sts=2 noet <bar> :retab! <bar> :set ts=4 sts=4 et <bar> :retab <CR>
                            imap \\ <ESC> :set ts=2 sts=2 noet <bar> :retab! <bar> :set ts=4 sts=4 et <bar> :retab <CR>

                            " Change movement keys to <space>wasd
                            noremap <Space-a> h
                            noremap <Space-s> j
                            noremap <Space-w> k
                            noremap <Space-d> l

                            " Set Paste
                            command SMP :set paste <CR>

                            " Set NoPaste
                            command SNP :set nopaste <CR>

                            " Tab to insert
                            map <TAB> i <TAB>

                            " Vim Signit
                            let g:signit_initials = "JR"
                            let g:signit_name = "Jeet Ray"
                            " let g:signit_extra_1
                            " let g:signit_extra_2
                            " let g:signit_position
                            let g:signit_ascii_font = "isometric1.flf"
                            " let g:signit_ascii_spacing

                            if has('nvim')
                                call dein#add('iron-e/nvim-libmodal')
                                call dein#add('shougo/deol.nvim')
                                call dein#add('shougo/deoppet.nvim')
                                call dein#add('shougo/deorise.nvim')
                            else
                                call dein#add('iron-e/vim-libmodal')
                                call dein#add('roxma/nvim-yarp')
                                call dein#add('roxma/vim-hug-neovim-rpc')
                            endif

                            let g:deoplete#enable_at_startup = 1
                        endfunction
                      '';
                    };
                    fish = {
                      enable = true;
                      plugins = with fishPlugins; [
                        { inherit (bass) name src; }
                        {
                          name = "nix-env";
                          src = inputs.nix-env-fish;
                        }
                      ];
                      shellAliases.s = "source ${
                          cfgx."fish/config.fish".source
                        }/.config/fish/config.fish";
                      shellInit = "bass source ${proFile}";
                    };
                    starship = {
                      enable = true;
                      enableBashIntegration = true;
                      enableFishIntegration = true;
                      enableIonIntegration = true;
                      enableElvishIntegration = true;
                      enableNushellIntegration = true;
                      enableZshIntegration = true;
                      enableXonshIntegration = true;
                      settings = let
                        red = "#FFADAD";
                        orange = "#FFD6A5";
                        yellow = "#FDFFB6";
                        green = "#CAFFBF";
                        cyan = "#9BF6FF";
                        blue = "#A0C4FF";
                        purple = "#BDB2FF";
                        pink = "#FFC6FF";
                        white = "#FFFFFC";
                        grey = "#222222";
                      in {
                        format = ''
                          [ ](bg:${red})\
                          $username\
                          [](bg:${orange} fg:${red})\
                          $env_var\
                          [](bg:${yellow} fg:${orange})\
                          $directory\
                          [](bg:${green} fg:${yellow})\
                          $git_branch\
                          $git_status\
                          [](bg:${cyan} fg:${green})\
                          $nix_shell\
                          [](bg:${blue} fg:${cyan})\
                          $c\
                          $elixir\
                          $elm\
                          $golang\
                          $haskell\
                          $java\
                          $julia\
                          $nodejs\
                          $nim\
                          $rust\
                          [](bg:${purple} fg:${blue})\
                          $docker_context\
                          [](bg:${pink} fg:${purple})\
                          $time\
                          [ ](fg:${pink})\
                        '';
                        username = rec {
                          show_always = true;
                          style_user = "bg:${red} fg:${grey}";
                          style_root = style_user;
                          format = "[$user ]($style)";
                        };
                        env_var.STARSHIP_SHELL = {
                          style = "bg:${orange} fg:${grey}";
                          format = "[ $env_value ]($style)";
                        };
                        directory = {
                          style = "bg:${yellow} fg:${grey}";
                          format = "[ $path ]($style)";
                          truncation_length = 3;
                          truncation_symbol = "…/";

                          substitutions = {
                            Documents = " ";
                            Downloads = " ";
                            Music = " ";
                            Pictures = " ";
                          };
                        };
                        nix_shell = {
                          symbol = " ";
                          style = "bg:${cyan} fg:${grey}";
                          impure_msg = "impure";
                          pure_msg = "pure";
                          format =
                            "[[ $symbol $state( ($name)) ](bg:${cyan} fg:${grey})]($style)";
                        };
                        c = {
                          symbol = " ";
                          style = "bg:${blue} fg:${grey}";
                          format =
                            "[[ $symbol ($version) ](bg:${blue} fg:${grey})]($style)";
                        };
                        docker_context = {
                          symbol = " ";
                          style = "bg:${purple} fg:${grey}";
                          format =
                            "[[ $symbol $context ](bg:${purple} fg:${grey})]($style) $path";
                        };
                        elixir = {
                          symbol = " ";
                          style = "bg:${blue} fg:${grey}";
                          format =
                            "[[ $symbol ($version) ](bg:${blue} fg:${grey})]($style)";
                        };
                        elm = {
                          symbol = " ";
                          style = "bg:${blue} fg:${grey}";
                          format =
                            "[[ $symbol ($version) ](bg:${blue} fg:${grey})]($style)";
                        };
                        git_branch = {
                          symbol = "";
                          style = "bg:${green} fg:${grey}";
                          format =
                            "[[ $symbol $branch ](bg:${green} fg:${grey})]($style)";
                        };
                        git_status = {
                          style = "bg:${green} fg:${grey}";
                          format =
                            "[[($all_status$ahead_behind )](bg:${green} fg:${grey})]($style)";
                        };
                        golang = {
                          symbol = " ";
                          style = "bg:${blue} fg:${grey}";
                          format =
                            "[[ $symbol ($version) ](bg:${blue} fg:${grey})]($style)";
                        };
                        haskell = {
                          symbol = " ";
                          style = "bg:${blue} fg:${grey}";
                          format =
                            "[[ $symbol ($version) ](bg:${blue} fg:${grey})]($style)";
                        };
                        java = {
                          symbol = " ";
                          style = "bg:${blue} fg:${grey}";
                          format =
                            "[[ $symbol ($version) ](bg:${blue} fg:${grey})]($style)";
                        };
                        julia = {
                          symbol = " ";
                          style = "bg:${blue} fg:${grey}";
                          format =
                            "[[ $symbol ($version) ](bg:${blue} fg:${grey})]($style)";
                        };
                        nodejs = {
                          symbol = "";
                          style = "bg:${blue} fg:${grey}";
                          format =
                            "[[ $symbol ($version) ](bg:${blue} fg:${grey})]($style)";
                        };
                        nim = {
                          symbol = " ";
                          style = "bg:${blue} fg:${grey}";
                          format =
                            "[[ $symbol ($version) ](bg:${blue} fg:${grey})]($style)";
                        };
                        rust = {
                          symbol = "";
                          style = "bg:${blue} fg:${grey}";
                          format =
                            "[[ $symbol ($version) ](bg:${blue} fg:${grey})]($style)";
                        };
                        time = {
                          disabled = false;
                          time_format = "%R";
                          style = "bg:${pink} fg:${grey}";
                          format =
                            "[[ ♥ $time ](bg:${pink} fg:${grey})]($style)";
                        };
                      };
                    };
                    zsh = {
                      autocd = true;
                      defaultKeymap = "vicmd";
                      shellAliases.s =
                        "source ${cfg.home."${relToZshDir}/.zshrc".source}";
                      initExtraFirst = ''
                        source ${inputs.grml}/etc/zsh/zshrc
                        source ${proFile}
                        prompt off
                      '';
                      enableAutosuggestions = true;
                      enableSyntaxHighlighting = true;
                      historySubstringSearch.enable = true;
                      envExtra = ''
                        source ${inputs.grml}/etc/zsh/zshenv
                        skip_global_compinit=1
                      '';
                      profileExtra = "source ${inputs.grml}/etc/zsh/zprofile";
                      logoutExtra = "source ${inputs.grml}/etc/zsh/zlogout";
                      loginExtra = "source ${inputs.grml}/etc/zsh/zlogin";
                      history = {
                        ignoreDups = true;
                        ignoreSpace = true;
                        expireDuplicatesFirst = true;
                      };
                      shellOptions = [
                        "EXTENDED_HISTORY"
                        # "HIST_EXPIRE_DUPS_FIRST"
                        # "HIST_IGNORE_DUPS"
                        "HIST_IGNORE_ALL_DUPS"
                        # "HIST_IGNORE_SPACE"
                        "HIST_FIND_NO_DUPS"
                        "HIST_SAVE_NO_DUPS"
                      ];
                      initExtra = ''
                        rc () { eval $(history | ${gnused}/bin/sed 's/^ *[0-9]* *//' | ${fzf-tmux}); }
                        bindkey -v
                      '';
                    };
                    bash = {
                      inherit (cfg.home) sessionVariables;
                      enable = true;
                      enableCompletion = true;
                      enableVteIntegration = true;
                      historyControl =
                        splitString ":" cfg.home.sessionVariables.HISTCONTROL;
                      shellOptions = toList "vi";
                      shellAliases.s =
                        "source ${cfg.home.file.".bashrc".source}";
                      bashrcExtra = profile;
                      profileExtra = ''
                        source ${homeDirectory}/.nix-profile/etc/profile.d/nix.sh || :
                        eval "$(${pkgs.fasd}/bin/fasd --init auto)"
                        eval "$(${zoxide} init posix --hook prompt)"

                        cdf () { cd $(${pkgs.fasd}/bin/fasd -ld | ${fzf-tmux}); }
                        cdi () { cd $(getFzfdfOutput "$@" "-t" "d"); }
                        cdr () { cd $(${zoxide} query -l | ${fzf-tmux}); }
                        direnvAllow () {
                            if [ -z "$1" ]; then
                                ${direnv} allow
                            else
                                for d in "$@"; do
                                    ${direnv} allow "$d"
                                done
                            fi
                        }
                        getFzfdfOutput () {
                            if [ -z "$1" ]; then
                                echo $(${fd}/bin/fd | ${fzf-tmux})
                            else
                                if [ -d "$1" ]; then
                                    echo $(${fd}/bin/fd "." "$@" | ${fzf-tmux})
                                else
                                    echo $(${fd}/bin/fd "$@" | ${fzf-tmux})
                                fi
                            fi
                        }
                        mdg () { mkdir -p "$@" && cd "$1"; }

                        Run () { curl --create-dirs -fsSLo "$2" "$1" && shift && run "$@"; }
                        run () { chmod +x "$1" && "$@"; }
                        rc () { eval $(history | ${gnused}/bin/sed 's/^ *[0-9]* *//' | ${fzf-tmux}); }
                      '';
                    };
                    home-manager.enable = true;
                    kitty = {
                      enable = true;
                      extraConfig =
                        "include ${inputs.kitty-themes}/kittyThemes/themes/Monokai_Pro_(Filter_Ristretto).conf";
                      font = mkIf (pkgs ? cartograph-cf-all) {
                        package = pkgs.cartograph-cf-all;
                        name = "Cartograph CF";
                      };
                    };
                    tmux = {
                      enable = true;
                      tmuxp.enable = true;
                      newSession = true;
                      aggressiveResize = true;
                      historyLimit = 50000;
                      displayTime = 1250;
                      keyMode = "vi";
                      prefix = "None";
                      escapeTime = 0;
                      plugins = with pkgs.tmuxPlugins; [
                        battery
                        cpu
                        ctrlw
                        fpp
                        fzf-tmux-url
                        jump
                        logging
                        online-status
                        open
                        pain-control
                        prefix-highlight
                        safekill
                        sessionist
                        sidebar
                        sidebar-plus
                        sysstat
                        thumbs
                        tmux-fzf
                        vim-tmux-focus-events
                        vim-tmux-navigator
                        yank
                        {
                          plugin = continuum;
                          extraConfig = ''
                            set -g @continuum-boot 'on'
                            set -g @continuum-save-interval '10'
                            set -g @continuum-restore 'on'
                          '';
                        }
                        {
                          plugin = resurrect;
                          extraConfig = ''
                            set -g @resurrect-save-bash-history 'on'
                            set -g @resurrect-dir '${homeDirectory}/.byobu/tmux_resurrect'
                            set -g @resurrect-processes '${
                              toString [
                                ''"mc --nocolor"''
                                ''"tail -f"''
                                "bat"
                                "docker"
                                "elvish"
                                "emacs"
                                "fish"
                                "glances"
                                "gotop"
                                "htop"
                                "ion"
                                "ipython"
                                "jupyter-lab"
                                "jupyter-notebook"
                                "less"
                                "man"
                                "more"
                                "mosh"
                                "mutt"
                                "nvim"
                                "ssh"
                                "syncthing"
                                "tail"
                                "top"
                                "vi"
                                "vim"
                                "weechat"
                                "woman"
                                "wtf"
                                "xonsh"
                                "xsh"
                                "zsh"
                              ]
                            }'
                            set -g @resurrect-capture-pane-contents 'on'
                            set -g @resurrect-strategy-vim 'session'
                            set -g @resurrect-strategy-nvim 'session'
                          '';
                        }
                      ];
                      extraInit =
                        replaceStrings [ "unbind C-b" "set -g prefix C-a" ] [
                          "# unbind C-b"
                          "set -g prefix C-Space"
                        ] (readFile "${inputs.aleclearmind}/active-row.conf");
                      sources = [
                        # "${inputs.powerline}/powerline/bindings/tmux/powerline-base.conf"
                        "${inputs.powerline}/powerline/bindings/tmux/powerline.conf"
                        # "${inputs.powerline}/powerline/bindings/tmux/powerline_tmux_2.1_plus.conf"
                        "${inputs.oh-my-tmux}/.tmux.conf"
                        "${inputs.oh-my-tmux}/.tmux.conf.local"
                      ];
                      unbindings = [ "C-b" "C-x" "C-z" ];
                      root-repeating-bindings = {
                        M-s = "send-prefix";
                        C-S-F5 = "send-keys M-F5";
                        C-S-Left = "send-keys M-Left";
                        C-S-Right = "send-keys M-Right";
                        C-Space = ''
                          {
                                                                      set key-table prefix
                                                                      set status-bg yellow
                                                                  }'';
                      };
                      prefix-table-bindings = {
                        C-Space = ''
                          {
                                                                      set key-table root
                                                                      set status-bg green
                                                                  }'';
                      };
                      set-globally = {
                        prefix2 = "S-Space";
                        visual-activity = "off";
                        status-justify = "centre";
                        status-right =
                          "'#{prefix_highlight} | %a %Y-%m-%d %H:%M'";
                        focus-events = "on";
                        pane-active-border-style = ''"bg=default"'';
                        utf8 = "on";
                        status-utf8 = "on";
                        status-interval = 1;
                      };
                      set-and-append-globally.pane-active-border-style =
                        ''"fg=colour208"'';
                      set-window-globally = { monitor-activity = "off"; };
                      shells-to-run = toList "powerline-daemon -q";
                      set-server-globally.escape-time = 10;
                    };
                  };
                  systemd.user.services = {
                    emacs-damascus = {
                      Unit = {
                        Description = "Emacs text editor";
                        Documentation = [
                          "info:emacs"
                          "man:emacs(1)"
                          "https://gnu.org/software/emacs/"
                        ];
                      };
                      Service = {
                        Type = "forking";
                        ExecStart =
                          "${cfg.services.emacs.package}/bin/emacs --bg-daemon=damascus --update";
                        ExecStop =
                          ''${emacsclient} -s damascus -e "(kill-emacs)"'';
                        Environment = "SSH_AUTH_SOCK=%t/keyring/ssh";
                        Restart = "on-failure";
                        TimeoutSec = 900;
                      };
                      Install.WantedBy =
                        mkIf (!cfg.services.emacs.enable) "default.target";
                    };
                  };
                  fonts.fontconfig.enable = true;
                  gtk = {
                    enable = true;
                    cursorTheme = {
                      name = "Oreo_dracula_orange_cursors";
                      package = pkgs.oreo-custom-cursors;
                    };
                    font = mkIf (pkgs ? cartograph-cf-all) {
                      enable = true;
                      package = pkgs.cartograph-cf-all;
                      name = "Cartograph CF";
                    };
                    theme = {
                      package = pkgs.dracula-theme;
                      name = "Dracula";
                    };
                  };
                  home = {
                    inherit homeDirectory;
                    activation.setup-yubikey-sudo =
                      lib.hm.dag.entryAfter [ "writeBoundary" ]
                      "$DRY_RUN_CMD ykpamcfg -2 -v";
                    enableDebugInfo = true;
                    enableNixpkgsReleaseCheck = true;
                    stateVersion = bundle.baseVersion;
                    packages = [

                    ];
                    shellAliases = rec {
                      "-" = "pushd";
                      ".." = "cd ..";
                      la =
                        "${cfg.programs.exa.package}/bin/exa -la --octal-permissions";
                      "." = la;
                      c = "clear";
                      emd = "systemctl --user start emacs.service";
                      kemd = "systemctl --user stop emacs.service";
                      git = "${pkgs.hub}/bin/hub";
                      md = "mkdir -p";
                      mosh =
                        "${pkgs.mosh}/bin/mosh --experimental-remote-ip=remote";
                      n = "exit";
                      remd = "systemctl --user restart emacs.service";
                      semd = "systemctl status emacs";
                      # fasd
                      o = "${pkgs.fasd}/bin/fasd -ae xdg-open";

                      # quick opening files with emacs
                      e = "${pkgs.fasd}/bin/fasd -fe '${emacsclient} -t'";

                      s = "source ${cfg.home.file.".profile".source}";

                      # Functions
                      cdf = "cdf";
                      cdi = "cdi";
                      cdr = "cdr";
                      da = "direnvAllow";
                      mdg = "mdg";
                      rc = "rc";
                      Run = "Run";
                      run = "run";
                    };
                    sessionVariables = rec {
                      EDITOR = "${emacsclient} -c";
                      VISUAL = EDITOR;
                      HISTCONTROL = "erasedups:ignoredups:ignorespace";
                      LESSOPEN =
                        "| {pkgs.sourceHighlight}/bin/src-hilite-lesspipe.sh %s";
                      LESS = " -R ";
                    };
                    sessionPath = flatten [
                      (map (slashcat homeDirectory) [
                        ".local/bin"
                        ".nimble/bin"
                        ".nix-profile/bin"
                        ".guix-profile/bin"
                        "go/bin"
                      ])
                      (map (sconcat "/") [
                        "usr/local/sbin"
                        "usr/local/bin"
                        "usr/sbin"
                        "usr/bin"
                        "sbin"
                        "bin"
                        "usr/games"
                        "usr/local/games"
                        "snap/bin"
                        "usr/local/go/bin"
                        "usr/lib/node_modules"
                      ])
                    ];
                    file = {
                      user.source = inputs.user;
                      ".gitattributes" = {
                        inherit (cfgx."git/attributes") source;
                      };
                      ".gitignore" = { inherit (cfgx."git/ignore") source; };
                      ".hgignore" = { inherit (cfgx."git/ignore") source; };
                      ".config/powershell/Microsoft.PowerShell_profile.ps1".text =
                        ''
                          # For zoxide v0.8.0+
                          Invoke-Expression (& {
                              $hook = if ($PSVersionTable.PSVersion.Major -lt 6) { 'prompt' } else { 'pwd' }
                              (${zoxide} init --hook $hook powershell | Out-String)
                          })
                          Invoke-Expression (&${starship} init powershell)
                        '';
                      ".byobu/.tmux.conf".source = cfgx."tmux/tmux.conf";
                      ".tmuxp/default.yaml".text = "session_name: default";
                      ".xinitrc".text = ''exec ${emacsclient} -a "" -c'';
                    };
                  };
                  services = {
                    qtile = {
                      enable = true;
                      extraInit = ''
                        # Copyright (c) 2010 Aldo Cortesi
                        # Copyright (c) 2010, 2014 dequis
                        # Copyright (c) 2012 Randall Ma
                        # Copyright (c) 2012-2014 Tycho Andersen
                        # Copyright (c) 2012 Craig Barnes
                        # Copyright (c) 2013 horsik
                        # Copyright (c) 2013 Tao Sauvage
                        #
                        # Permission is hereby granted, free of charge, to any person obtaining a copy
                        # of this software and associated documentation files (the "Software"), to deal
                        # in the Software without restriction, including without limitation the rights
                        # to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
                        # copies of the Software, and to permit persons to whom the Software is
                        # furnished to do so, subject to the following conditions:
                        #
                        # The above copyright notice and this permission notice shall be included in
                        # all copies or substantial portions of the Software.
                        #
                        # THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
                        # IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
                        # FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
                        # AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
                        # LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
                        # OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
                        # SOFTWARE.
                      '';
                      fromImports = {
                        libqtile = {
                          config =
                            [ "Key" "Screen" "Group" "Drag" "Click" "Match" ];
                          command = "lazy";
                          dgroups = "simple_key_binder";
                          null = [ "layout" "bar" "widget" ];
                        };
                      };
                      keys = [
                        {
                          key = "j";
                          action = "lazy.layout.down()";
                        }
                        {
                          key = "k";
                          action = "lazy.layout.up()";
                        }
                        {
                          mod = "control";
                          key = "j";
                          action = "lazy.layout.shuffle_down()";
                        }
                        {
                          mod = "control";
                          key = "k";
                          action = "lazy.layout.shuffle_up()";
                        }
                        {
                          key = "space";
                          action = "lazy.layout.next()";
                        }
                        {
                          key = "Left";
                          action = "lazy.screen.prevgroup()";
                        }
                        {
                          key = "Right";
                          action = "lazy.screen.nextgroup()";
                        }
                        {
                          mod = "shift";
                          key = "space";
                          action = "lazy.layout.rotate()";
                        }
                        {
                          mod = "shift";
                          key = "Return";
                          action = "lazy.layout.toggle_split()";
                        }
                        {
                          key = "Tab";
                          action = "lazy.nextlayout()";
                        }
                        {
                          key = "w";
                          action = "lazy.window.kill()";
                        }
                        {
                          key = "Return";
                          action = ''lazy.spawn("urxvt")'';
                        }
                        {
                          mod = "control";
                          key = "r";
                          action = "lazy.restart()";
                        }
                        {
                          mod = "control";
                          key = "q";
                          action = "lazy.shutdown()";
                        }
                        {
                          key = "r";
                          action = "lazy.spawncmd()";
                        }
                      ];
                      groups = map (g:
                        if (isString g) then
                          " ${g} "
                        else
                          (g // { name = " ${g.name} "; })) ([
                            "urxvt"
                            {
                              name = "web";
                              matches = [{ wm_class = "Firefox"; }];
                            }
                            "blender"
                            "inkscape"
                            "gimp"
                            "doc"
                          ]);
                      layouts = [
                        {
                          function = "TreeTab";
                          args = {
                            font = "Cartograph CF Light Italic";
                            name = "tree tab";
                            bg_color = "#222222";
                            inactive_bg = "#AB5DEE";
                            panel_width = 150;
                            margin_left = 0;
                            margin_y = 0;
                            sections = [ "TreeTab" ];
                            section_left = 0;
                            padding_x = 4;
                            active_bg = "#FFB86C";
                            rounded = false;
                          };
                        }
                        {
                          function = "MonadTall";
                          args = {
                            name = "xmonad tall";
                            ratio = 0.5;
                            border_width = 8;
                            border_focus = "#335260";
                            border_normal = "#69B2B8";
                          };
                        }
                        {
                          function = "Stack";
                          args = {
                            num_stacks = 2;
                            border_width = 8;
                            border_focus = "#335260";
                            border_normal = "#69B2B8";
                          };
                        }
                        {
                          function = "Floating";
                          args = {
                            name = "floating";
                            border_width = 8;
                            border_focus = "#335260";
                            border_normal = "#69B2B8";
                          };
                        }
                      ];
                      mouse = [
                        {
                          function = "Drag";
                          args = [
                            [ (f: "mod") ]
                            "Button1"
                            (f: "lazy.window.set_position_floating()")
                          ];
                          kwargs.start = f: "lazy.window.get_position()";
                        }
                        {
                          function = "Click";
                          args = [
                            [ (f: "mod") ]
                            "Button2"
                            (f: "lazy.window.bring_to_front()")
                          ];
                        }
                        {
                          function = "Drag";
                          args = [
                            [ (f: "mod") ]
                            "Button3"
                            (f: "lazy.window.set_size_floating()")
                          ];
                          kwargs.start = f: "lazy.window.get_size()";
                        }
                      ];
                      extraConfig = ''
                        floating_layout = layout.Floating(
                            name="floating",
                            border_width=8,
                            border_focus="#69B2B8",
                            border_normal="#335260",
                        )

                        widget_defaults = dict(
                            font='Cartograph CF Light Italic',
                            fontsize=12,
                            background="#222222",
                            markup=True,
                        )

                        screens = [
                            Screen(
                                bottom=bar.Bar(
                                    [
                                        widget.GroupBox(
                                                borderwidth=0,
                                                margin=0,
                                                padding=6,
                                                active="FFFFFF",
                                                inactive="FFB86C",
                                                highlight_method="block",
                                                this_current_screen_border="#AB5DEE",
                                                invert_mouse_wheel=True,
                                                rounded=False,
                                            ),
                                        widget.Prompt(),
                                        widget.CurrentLayout(
                                                background="#E11B22",
                                            ),
                                        widget.Spacer(),
                                        #widget.WindowName(),
                                        widget.TextBox("testing", name="default"),
                                        widget.Systray(),
                                        widget.Clock(format=' %I:%M %p '),
                                    ],
                                    24,
                                    background="#335260",
                                ),
                            ),
                        ]
                      '';
                      variables = {
                        dgroups_app_rules = [ ];
                        main = null;
                        follow_mouse_focus = true;
                        bring_front_click = false;
                        cursor_warp = false;
                        auto_fullscreen = true;
                        wmname = "LG3D";
                      };
                    };
                    emacs = {
                      enable = true;
                      client = {
                        enable = true;
                        arguments = toList "--update";
                      };
                      defaultEditor = true;
                    };
                  };
                }) bundle.attrs.allUsers)
              {
                ${bundle.attrs.allUsers.primary} = {
                  programs = {
                    borgmatic = {
                      enable = true;
                      backups = let
                        shared = {
                          consistency = {
                            checks = [
                              {
                                name = "repository";
                                frequency = "2 weeks";
                              }
                              {
                                name = "archives";
                                frequency = "4 weeks";
                              }
                              {
                                name = "data";
                                frequency = "6 weeks";
                              }
                              {
                                name = "extract";
                                frequency = "6 weeks";
                              }
                            ];
                          };
                          location.extraConfig = rec {
                            one_file_system = false;
                            numeric_owner = false;
                            atime = true;
                            ctime = true;
                            birthtime = true;
                            read_special = true;
                            bsd_flags = true;
                            files_cache = "ctime,size,inode";
                            local_path = "${pkgs.borg}/bin/borg";
                            remote_path = local_path;
                            exclude_caches = true;
                            exclude_nodump = true;
                            exclude_patterns = toList ".zfs/snapshot";
                          };
                          retention = {
                            keepDaily = 7;
                            keepHourly = 24;
                            keepMinutely = 60;
                            keepMonthly = 6;
                            keepSecondly = 60;
                            keepWeekly = 4;
                            keepWithin = "6H";
                            keepYearly = 4;
                          };
                          storage = {
                            checkpoint_interval = 300;
                            compression = "auto,zstd,22";
                            ssh_command = cfg.programs.assh.alias;
                            relocated_repo_access_is_ok = true;
                            unknown_unencrypted_repo_access_is_ok = true;
                          };
                        };
                        nameShared = name: rec {
                          storage = {
                            encryptionPasscommand =
                              "pass show backup/borg/${name}";
                            extraConfig.archive_name_format =
                              "borgmatic-${name}-{now:%%Y%%m%%dT%%H%%M%%S%%f}";
                          };
                          location.extraConfig.hooks.before_backup = mkBefore [
                            "${pkgs.password-store}/bin/pass show backup/borg/${name} &> /dev/null || exit 75"
                          ];
                          retention.extraConfig.prefix = "borgmatic-${name}-";
                          consistency = { inherit (retention) extraConfig; };
                        };
                      in mapAttrs (n: v: mkMerge [ shared (nameShared n) v ]) {
                        oreo = {
                          location = {
                            sourceDirectories = toList /chimchar/oreo;
                            repositories = toList /oreo;
                            extraConfig.hooks = {
                              before_backup =
                                map (sepcat "&> /dev/null || exit 75") [
                                  "findmnt /chimchar/oreo"
                                  "findmnt /oreo"
                                ];
                              healthchecks =
                                "https://hc-ping.com/9660799a-0aba-44d7-a29f-3887a0ce82bd";
                            };
                          };
                        };
                        oreo-rsync = {
                          location = {
                            sourceDirectories = toList /chimchar/oreo;
                            repositories =
                              toList "9237@usw-s009.rsync.net/./oreo-rsync";
                            extraConfig.hooks = {
                              remote_path = "borg1";
                              before_backup =
                                map (sepcat "&> /dev/null || exit 75") [
                                  "findmnt /chimchar/oreo"
                                  "ping -qc 1 usw-s009.rsync.net"
                                ];
                              healthchecks =
                                "https://hc-ping.com/e6d79f19-8c5b-429a-99d0-0247fdb251ea";
                            };
                          };
                        };
                        infernape = {
                          location = {
                            sourceDirectories = toList /chimchar;
                            repositories = toList /infernape;
                            extraConfig.hooks = {
                              remote_path = "borg1";
                              before_backup =
                                map (sepcat "&> /dev/null || exit 75") [
                                  "${mountable-mounted} chimchar"
                                  "findmnt /infernape"
                                ];
                              healthchecks =
                                "https://hc-ping.com/aa90aa9a-d507-4c8e-92c9-a037cd42e585";
                            };
                          };
                        };
                        user = {
                          location = {
                            sourceDirectories = toList inputs.user;
                            repositories = [
                              "/home/shadowrylander/.user"
                              "hpvlk40u@hpvlk40u.repo.borgbase.com/./repo"
                            ];
                            extraConfig.hooks = {
                              before_backup =
                                map (sepcat "&> /dev/null || exit 75")
                                [ "ping -qc 1 hpvlk40u.repo.borgbase.com" ];
                              healthchecks =
                                "https://hc-ping.com/1ed0af6c-aa1d-4930-aa1e-3d1af89c9251";
                            };
                          };
                        };
                      };
                    };
                  };
                };
              }
            ];
          };
        };
      };
    };
}
