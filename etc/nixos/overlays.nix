args@{ lib, nixpkgs, inputs, pkgs, channel }: with builtins; with lib;
let
    updatePython = pv: prev: attrs: { "${pv}" = prev.${pv}.override { packageOverrides = new: old: attrs; }; };
    updatePythonPackages = pv: final: prev: dir: updatePython pv prev (j.import.set { call = final.${pv}.pkgs; inherit dir; ignores = j.dirCon.dirs dir; });
    pv2 = "python2${j.attrs.versions.python.two}";
    pv3 = "python3${j.attrs.versions.python.three}";
in flatten [
(final: prev: { j = { inherit pkgs; };})
# (final: prev: lib.attrNames (lib.filterAttrs (n: v: builtins.all (b: b == true) [ (! builtins.elem n [ ]) (builtins.tryEval v).success (v ? patchRegistryDeps) ]) pkgs))
(final: prev: genAttrs [ "afterburn" "agate" "airshipper" "ajour" "alacritty" "alass" "alfis" "alfis-nogui" "amber" "amber-secret" "amp" "anevicon" "anewer" "angle-grinder" "anup" "apkeep" "as-tree" "asciinema-scenario" "asuka" "async" "atuin" "authoscope" "b3sum" "bacon" "badtouch" "bandwhich" "bat" "bingrep" "bitwarden_rs" "bitwarden_rs-mysql" "bitwarden_rs-postgresql" "bitwarden_rs-sqlite" "blflash" "blightmud" "blightmud-tts" "bore" "boringtun" "bottom" "bottom-rs" "bpb" "break-time" "broot" "bukubrow" "bunyan-rs" "bupstash" "cached-nix-shell" "cargo" "cargo-about" "cargo-all-features" "cargo-asm" "cargo-audit" "cargo-binutils" "cargo-bisect-rustc" "cargo-bitbake" "cargo-bloat" "cargo-c" "cargo-cache" "cargo-crev" "cargo-criterion" "cargo-cross" "cargo-deadlinks" "cargo-deb" "cargo-deny" "cargo-depgraph" "cargo-dephell" "cargo-deps" "cargo-diet" "cargo-edit" "cargo-embed" "cargo-expand" "cargo-feature" "cargo-flamegraph" "cargo-flash" "cargo-fund" "cargo-fuzz" "cargo-geiger" "cargo-generate" "cargo-graph" "cargo-inspect" "cargo-insta" "cargo-kcov" "cargo-license" "cargo-limit" "cargo-llvm-lines" "cargo-make" "cargo-modules" "cargo-msrv" "cargo-outdated" "cargo-play" "cargo-raze" "cargo-readme" "cargo-release" "cargo-rr" "cargo-sort" "cargo-spellcheck" "cargo-supply-chain" "cargo-sweep" "cargo-sync-readme" "cargo-tally" "cargo-tarpaulin" "cargo-udeps" "cargo-update" "cargo-valgrind" "cargo-watch" "cargo-web" "cargo-whatfeatures" "cargo-wipe" "cargo-xbuild" "castor" "catfs" "cfdyndns" "chars" "chit" "choose" "cicero-tui" "click" "clipcat" "clippy" "cliscord" "cloak" "clog-cli" "cloud-hypervisor" "cntr" "cobalt" "cocom" "code-minimap" "colmena" "coloursum" "commit-formatter" "convco" "crabz" "crate2nix" "crosvm" "csview" "czkawka" "delta" "deno" "devserver" "diesel-cli" "diffr" "difftastic" "dijo" "diskonaut" "diskus" "dnspeep" "dogdns" "doh-proxy-rust" "dot-http" "dotenv-linter" "downonspot" "dprint" "drill" "du-dust" "dua" "dutree" "dwfv" "dwm-status" "each" "ecpdap" "effitask" "eidolon" "elan" "electrs" "elfcat" "elfx86exts" "emplace" "emulsion" "epick" "espanso" "ethabi" "eureka-ideas" "eva" "evcxr" "evscript" "eww" "eww-wayland" "exa" "fac-build" "fastmod" "fclones" "fcp" "fd" "fedigroups" "fend" "ffsend" "fido2luks" "finalfrontier" "finalfusion-utils" "findomain" "firmware-manager" "fishfight" "fishnet" "fitnesstrax" "flavours" "flip-link" "fnm" "fontfor" "freenukum" "freshfetch" "fselect" "fst" "fundoc" "g933-utils" "geckodriver" "genact" "genpass" "gifski" "gir-rs" "git-absorb" "git-backup" "git-branchless" "git-cliff" "git-codeowners" "git-dit" "git-gone" "git-ignore" "git-interactive-rebase-tool" "git-quickfix" "git-series" "git-subset" "git-trim" "git-vanity-hash" "git-workspace" "gitoxide" "gitui" "gleam" "glitter" "gnirehtet" "gnvim-unwrapped" "gobang" "gotify-desktop" "gpg-tui" "gping" "gptman" "graphql-client" "grex" "gutenberg" "habitat" "hacksaw" "handlr" "hck" "heatseeker" "helix" "helvum" "hexdino" "hexyl" "hiksink" "himalaya" "hors" "ht-rust" "htmlq" "httplz" "hurl" "hydra-cli" "hyperfine" "i3-auto-layout" "i3-ratiosplit" "i3nator" "i3status-rust" "i3wsr" "imag" "image-roll" "inferno" "inherd-quake" "innernet" "inputplug" "intecture-agent" "intecture-auth" "intecture-cli" "intermodal" "ion" "itm-tools" "jless" "joshuto" "journaldriver" "jql" "jrsonnet" "just" "jwt-cli" "kak-lsp" "kalker" "kbs2" "keyscope" "kibi" "kile-wl" "kmon" "kondo" "krankerl" "krapslog" "kubernix" "kubie" "leftwm" "lemmy-server" "lethe" "lfs" "libreddit" "librespot" "licensor" "lighthouse-steamvr" "loc" "loop" "lorri" "lscolors" "lsd" "lucky-commit" "lunatic" "lychee" "macchina" "maker-panel" "manix" "martin" "mask" "materialize" "maturin" "mcfly" "mdbook" "mdbook-graphviz" "mdbook-katex" "mdbook-mermaid" "mdcat" "mdctags" "mdsh" "mdzk" "meilisearch" "meli" "menyoki" "mhost" "microserver" "miniserve" "mmtc" "monolith" "movine" "mozwire" "mq-cli" "muso" "myxer" "natls" "navi" "nbtscanner" "ncgopher" "ncspot" "neovide" "netease-cloud-music-gtk" "netease-music-tui" "newsboat" "nix-doc" "nix-du" "nix-index-unwrapped" "nix-query-tree-viewer" "nix-simple-deploy" "nix-template" "nixdoc" "nixpkgs-fmt" "noaa-apt" "nomino" "nsh" "nushell" "nvfancontrol" "nym" "octofetch" "oha" "onefetch" "openethereum" "ouch" "ox" "oxipng" "pactorio" "page" "panamax" "panopticon" "parinfer-rust" "parity" "passerine" "pastel" "pax-rs" "pazi" "peep" "pict-rs" "pijul" "pinyin-tool" "pipe-rename" "pipes-rs" "piping-server-rust" "pipr" "polkadot" "powerline-rs" "pqrs" "pr-tracker" "prisma-engines" "probe-run" "procs" "prometheus-unbound-exporter" "prometheus-wireguard-exporter" "proton-caller" "prs" "psw" "ptags" "pueue" "py-spy" "pyo3-pack" "quill" "rage" "railcar" "rargs" "rates" "rav1e" "rbw" "rdedup" "rebazel" "reddsaver" "regenkfs" "relibc" "remkrom" "resvg" "rhack" "rink" "ripasso-cursive" "ripgrep" "ripgrep-all" "ristate" "rls" "rm-improved" "rnix-hashes" "rnix-lsp" "roogle" "routinator" "rpg-cli" "rq" "rqbit" "rs-git-fsmonitor" "rsclock" "rshijack" "rslint" "rtss" "ruffle" "runiq" "ruplacer" "rust-analyzer-unwrapped" "rust-bindgen" "rust-cbindgen" "rust-code-analysis" "rust-motd" "rust-petname" "rust-script" "rustc-demangle" "rustcat" "rustfmt" "rustracer" "rustscan" "rustup" "rustup-toolchain-install-master" "rusty-man" "rx" "s3rs" "sad" "safe-rm" "sandboxfs" "sccache" "scryer-prolog" "sd" "sd-switch" "selene" "sentry-cli" "sequoia" "shadowenv" "shadowsocks-rust" "sheesy-cli" "shell-hist" "shellharden" "shotgun" "sic-image-cli" "sigi" "silicon" "simple-http-server" "sirula" "sit" "skim" "sn0int" "sniffglue" "so" "songrec" "sozu" "spotify-tui" "spotifyd" "sqlx-cli" "starship" "statix" "steam-acf" "steam-tui" "stork" "stylua" "sub-batch" "suckit" "supertag" "surface-control" "svd2rust" "svgbob" "svlint" "svls" "swapview" "swayr" "swaywsr" "synapse-bt" "synth" "system-syzygy" "system76-firmware" "szyszka" "t-rec" "tab-rs" "tagref" "taizen" "taplo-cli" "taplo-lsp" "tarssh" "taskwarrior-tui" "tdns-cli" "tealdeer" "tectonic" "tensorman" "terminal-typeracer" "termplay" "termscp" "tex-match" "texlab" "textplots" "texture-synthesis" "tezos-rust-libs" "the-way" "tickrs" "tidy-viewer" "tiny" "todiff" "tokei" "topgrade" "tox-node" "tp-auto-kbbl" "tre-command" "tree-sitter" "treefmt" "tremor-rs" "trunk" "ttyper" "tunnelto" "tv" "tydra" "typos" "udpt" "unpfs" "unused" "uq" "uwc" "uwuify" "vaultwarden" "vaultwarden-mysql" "vaultwarden-postgresql" "vaultwarden-sqlite" "vector" "verco" "virtiofsd" "viu" "vivid" "void" "vopono" "wagyu" "wasm-bindgen-cli" "wasm-pack" "wasmer" "wasmtime" "watchexec" "webmetro" "websocat" "wezterm" "wg-bond" "wgpu-utils" "whitebox-tools" "wiki-tui" "wishbone-tool" "wmfocus" "workstyle" "wrangler" "wyvern" "xcolor" "xcp" "xh" "xidlehook" "xplr" "xprite-editor" "xsv" "xv" "xxv" "yabridgectl" "zcash" "zellij" "zenith" "zenith-nvidia" "zktree" "zola" "zoxide" "zz" ] (pkg: prev.${pkg}))
(final: prev: rec {
    python2 = final.${pv2};
    python3 = final.${pv3};
    python = final.python3;
})
(final: prev: updatePython pv3 prev { rich = prev.pythonPackages.rich.overridePythonAttrs (old: {
    version = "12.0.0";
    src = final.fetchFromGitHub {
        owner = "syvlorg";
        repo = old.pname;
        rev = "a6c20ce10adc7b8cfacfd74e0b025e8c2c8c19eb";
        sha256 = "1ld3ihvssfk56240wignmd6hv7gynid5wmcynl58ng8sbfywm3ly";
    };
    propagatedBuildInputs = (with final.pythonPackages; [ hy ]) ++ old.propagatedBuildInputs;
    meta = {
        description = "Render rich text, tables, progress bars, syntax highlighting, markdown and more to the terminal";
        homepage = "https://github.com/syvlorg/rich";
        license = lib.licenses.mit;
    };
}); })
(final: prev: { xonsh = prev.xonsh.overridePythonAttrs (old: { propagatedBuildInputs = (with final.pythonPackages; [ 
    bakery
    xontrib-sh
    xontrib-readable-traceback
    xontrib-pipeliner
    xonsh-autoxsh
    xonsh-direnv
]) ++ old.propagatedBuildInputs; }); })
(final: prev: { nur = import inputs.nur { nurpkgs = nixpkgs; pkgs = prev; }; })
inputs.emacs.overlay
(final: prev: let dir = ./callPackages; in j.import.set { call = true; inherit dir; ignores = j.dirCon.dirs dir; })
(final: prev: updatePythonPackages pv2 final prev ./callPackages/python2)
(final: prev: updatePythonPackages pv3 final prev ./callPackages/python3)
(final: prev: let dir = ./overlays; in j.import.set { inherit dir; ignores = j.dirCon.dirs dir; })
(let pkgsets = {
    # nixos-unstable = [ "gnome-tour" ];
    # nixos-unstable = "gnome-tour";
    # nixos-unstable = { python3 = "python310"; };
};
in mapAttrsToList (
    pkgchannel: pkglist': let
        pkglist = if (isString pkglist') then [ pkglist' ] else pkglist';
    in map (
        pkg': let
            pkgIsAttrs = isAttrs pkg';
            pkg1 = if pkgIsAttrs then (last (attrNames pkg')) else pkg';
            pkg2 = if pkgIsAttrs then (last (attrValues pkg')) else pkg';
            self = (pkgchannel == channel) || (pkgchannel == "self");
        in final: prev: { "${pkg1}" = if self then (if pkgIsAttrs then final.${pkg2} else prev.${pkg2}) else final.j.pkgs.${pkgchannel}.${pkg2}; }
    ) pkglist
) pkgsets)
(let pkgsets = {
    # nixos-unstable = [ { python310Packages = "mypy"; } { python310Packages = [ "mypy" ]; } ];
    # nixos-unstable = { python310Packages = "mypy"; };
    # nixos-unstable = { python310Packages = [ "mypy" ]; };
    # nixos-22-05 = { python310Packages = "mypy"; };
};
in mapAttrsToList (
    pkgchannel: pkglist': let
        pkglist = if (isAttrs pkglist') then [ pkglist' ] else pkglist';
    in map (
        pkg': let
            pkg1 = last (attrNames pkg');
            pkg2Pre = last (attrValues pkg');
            pkg2IsString = isString pkg2Pre;
            self = (pkgchannel == channel) || (pkgchannel == "self");
            pkgFunc = pkg: { "${pkg}" = if self then (if pkgIsAttrs then final.${pkg} else prev.${pkg}) else final.j.pkgs.${pkgchannel}.${pkg1}.${pkg}; };
            pkg2 = if pkg2IsString then (pkgFunc pkg2Pre) else (genAttrs pkg2Pre pkgFunc);
        in final: prev: { "${pkg1}" = pkg2; }
    ) pkglist
) pkgsets)
]
