from nanite import fullpath as fp
from os import path as os_path, sep as os_sep
from inspect import getsourcefile

def _yadm(args, stdin=None):
    if args[0] == "edit":
        vim @(fp(f"{args[1]}##template"))
    else:

        # From https://github.com/xonsh/xonsh/issues/3159#issuecomment-500364622
        $(which -s yadm) @(args)

    return 

def mdg(args, stdin=None):
    mkdir -p @(args)
    cd @(args[0])

def _vim(args, stdin=None):
    $(which -s vim) @(args)
