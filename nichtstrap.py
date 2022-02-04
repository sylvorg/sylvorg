import hy, os
from nanite import module_installed, sui
nichtstrap = module_installed(os.path.dirname(os.path.realpath(__file__)) + "/nichtstrap.hy")
if __name__ == "__main__":
    nichtstrap(obj=sui("addict", "Dict")(dict()))
