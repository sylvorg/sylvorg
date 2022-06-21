{ lib
, buildPythonPackage
, fetchPypi
, six
}:

buildPythonPackage rec {
  pname = "xontrib-pipeliner";
  version = "0.3.4";

  src = fetchPypi {
    inherit pname version;
    sha256 = "sha256-f8tUjPEQYbycq1b3bhXwPU2YF9fkp1URqDDLH2CeNpo=";
  };

  propagatedBuildInputs = [ six ];

  postPatch = ''
    substituteInPlace setup.py --replace "'xonsh', " ""
  '';

  meta = {
    description = "Let your pipe lines flow thru the Python code in xonsh.";
    homepage = "https://github.com/anki-code/xontrib-sh";
    license = lib.licenses.mit;
  };
}
