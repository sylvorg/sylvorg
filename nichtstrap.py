import hy
from addict import Dict
exec(+ (.dirname os.path (.realpath os.path __file__)) "nichtstrap.hy")
if __name__ == "__main__":
    nichtstrap(obj=Dict(dict()))
