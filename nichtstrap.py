#!/usr/bin/env python3
import hy, os
from addict import Dict
from oreo import module_installed
nichtstrap = module_installed(os.path.dirname(os.path.realpath(__file__)) + "/nichtstrap.hy").nichtstrap
if __name__ == "__main__":
    nichtstrap(obj=Dict(dict()))
