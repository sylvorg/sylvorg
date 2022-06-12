{ lib
, buildPythonPackage
, fetchPypi
}:

buildPythonPackage rec {
  pname = "xonsh-autoxsh";
  version = "0.3";

  src = fetchPypi {
    inherit pname version;
    sha256 = "sha256-qwXbNbQ5mAwkZ4N+htv0Juw2a3NF6pv0XpolLIQfIe4=";
  };

  meta = {
    description = "Automatically execute scripts for directories in Xonsh Shell.";
    homepage = "https://github.com/Granitosaurus/xonsh-autoxsh";
    license = lib.licenses.mit;
  };
}
