{ lib
, buildPythonPackage
, fetchPypi
}:

buildPythonPackage rec {
  pname = "xontrib-sh";
  version = "0.3.0";

  src = fetchPypi {
    inherit pname version;
    sha256 = "sha256-eV++ZuopnAzNXRuafXXZM7tmcay1NLBIB/U+SVrQV+U=";
  };

  meta = {
    description = "Paste and run commands from bash, zsh, fish, tcsh in xonsh shell.";
    homepage = "https://github.com/anki-code/xontrib-sh";
    license = lib.licenses.mit;
  };
}
