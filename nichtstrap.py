import hy
import os
exec(os.path.dirname(os.path.realpath(__file__)) + "/nichtstrap.hy")
if __name__ == "__main__":
    nichtstrap(obj=Dict(dict()))
