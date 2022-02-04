import hy
from addict import Dict
exec(os.path.dirname(os.path.realpath(__file__)) + "nichtstrap.hy")
if __name__ == "__main__":
    nichtstrap(obj=Dict(dict()))
