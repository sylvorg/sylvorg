import hy
import os
filename = os.path.dirname(os.path.realpath(__file__)) + "/nichtstrap.hy"
with open(filename, "rb") as source_file:
    code = compile(source_file.read(), filename, "exec")
exec(code, globals, locals)
if __name__ == "__main__":
    nichtstrap(obj=Dict(dict()))
