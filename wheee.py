#!/usr/bin/env python3.10
import argparse
import os
from sys import argv
from itertools import zip_longest
from subprocess import run
parser = argparse.ArgumentParser()
parser.add_argument("-D", "--directory", default="/etc/nixos")
parser.add_argument("-f", "--flake", action="store_true")
parser.add_argument("-F", "--file")
parser.add_argument("-c", "--command", default="switch")
parser.add_argument("-s", "--hash", action="store_true")
parser.add_argument("--use-hash")
parser.add_argument("-v", "--verbose", action="store_true")
parser.add_argument("-d", "--dry-run", action="store_true")
parser.add_argument("-i", "--install", action="store_true")
parser.add_argument("-H", "--host", required=True)
args, unknown = parser.parse_known_args(argv[1:])

def grouper(iterable, n, fillvalue=None):
    "Collect data into fixed-length chunks or blocks"
    # grouper('ABCDEFG', 3, 'x') --> ABC DEF Gxx"
    args = [iter([
        s.removeprefix("--") if s.startswith("--") else s for s in iterable
    ])] * n
    return zip_longest(*args, fillvalue=fillvalue)

unknown = dict(grouper(unknown, 2, ""))

unknown["host"] = args.host

commandPre = f"nix-instantiate --read-write-mode --show-trace --eval -E 'let j = import " + os.path.dirname(os.path.realpath(argv[0])) + "/etc/nixos/lib {  }; in j.hostName"

commandSet = "{ stc = j.attrs.default-stc // " + " { "
for k, v in unknown.items():
    commandSet += f'{k} = "{v}"; '
commandSet += "};"

hashCommand = " ".join([
    commandPre,
    commandSet,
    "}'",
])

flake = lambda _hash: f'--flake "{args.directory}#{_hash}"' if args.flake else ""

if args.install:
    # command = lambda _hash: f'nixos-install --impure --show-trace {flake(_hash)}'
    command = lambda _hash: f'''nix build \
        --option build-use-substitutes true \
        --option substitute true \
        --impure \
        --show-trace \
        {args.directory}#nixosConfigurations.{_hash}.config.system.build.toplevel && \
        nixos-install --system ./result'''
else:
    command = lambda _hash: f'''nixos-rebuild {args.command} \
        --option build-use-substitutes true \
        --option substitute true \
        --impure \
        --show-trace {flake(_hash)}'''

getHash = lambda: args.use_hash or run(hashCommand, shell = True, capture_output = True, text = True).stdout.strip().strip('\"')

if args.verbose or args.dry_run:
    if args.hash:
        print(hashCommand)
    else:
        _hash = getHash()
        print(command(getHash()))
if not args.dry_run:
    if args.hash:
        print(getHash())
    else:
        run(command(getHash()), shell = True)
