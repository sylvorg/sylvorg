import hy
from addict import Dict
from .nichtstrap import nichtstrap
if __name__ == "__main__":
    nichtstrap(obj=Dict(dict()))
