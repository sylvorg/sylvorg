from nanite import fullpath as fp
from os import path as os_path, sep as os_sep
from inspect import getsourcefile

def _yadm(args, stdin=None):
    if args[0] == "edit":
        vim @(fp(f"{args[1]}##template"))
    else:
        # From https://github.com/xonsh/xonsh/issues/3159#issuecomment-500364622
        $(which -s yadm) @(args)

def _mdg(args, stdin=None):
    mkdir -p @(args)
    cd @(args[0])

# quick opening files with vim
def _fasdv(args, stdin=None):
    fasd -fe vim @(args)

def hgsetup(args, stdin=None):
    hg bookmark master
    hg checkout master
    hg add .hgignore
    hg ci -m .hgignore
    hg ci -Am @(args)
    hg push

def _alacritty_change_themes(args, stdin=None):
    _alc = "alacritty"
    _alc_theme = fullpath(
        config_dir,
        _alc,
        "themes",
        args[0]+".yaml"
    )
    _alc_config = fullpath(
        "~",
        ".config",
        _alc,
        _alc+".yml"
    )
    rsync @(_alc_theme) @(_alc_config)

def _compile(args, stdin=None):
    clear
    name = args.pop(0)
    g++ @(name + ".cpp")
    ./a.out @(name) @(args)

def _compile_only(args, stdin=None):
    g++ @(args[0] + ".cpp")

def _compile_header(args, stdin=None):
    g++ @(args[0] + ".h")

def _compile_link(args, stdin=None):
    for arg in args:
        g++ @(arg + ".cpp") -c
    args = [arg + ".o" for arg in args]
    g++ @(args) -o a.out

def _direnv_allow(args, stdin=None):
    if args:
        for argument in args:
            direnv allow @(argument)
    else:
        direnv allow

def _la(args, stdin=None):
    if "-t" in args:
        args = list(args)
        args.remove("-t")
        exa -laT @(args)
    else:
        exa -la @(args)

def _git(args, stdin=None):
    $(which -s hub) @(args)