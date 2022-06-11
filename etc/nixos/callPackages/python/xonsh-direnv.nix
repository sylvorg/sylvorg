{ lib
, buildPythonPackage
, fetchPypi
}:

buildPythonPackage rec {
  pname = "xonsh-direnv";
  version = "1.5.0";

  src = fetchPypi {
    inherit pname version;
    sha256 = "sha256-OLjtGD2lX4Yf3aHrxCWmAbSPZnf8OuVrBu0VFbsna1Y=";
  };

  meta = {
    description = "xonsh extension for using direnv";
    homepage = "https://github.com/Granitosaurus/xonsh-direnv";
    license = lib.licenses.mit;
  };
}
