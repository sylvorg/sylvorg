{ lib
, buildPythonPackage
, fetchPypi
, colorama
, backtrace
}:

buildPythonPackage rec {
  pname = "xontrib-readable-traceback";
  version = "0.3.2";

  src = fetchPypi {
    inherit pname version;
    sha256 = "sha256-1D/uyiA3A1dn9IPakjighckZT5Iy2WOMroBkLMp/FZM=";
  };

  propagatedBuildInputs = [ colorama backtrace ];

  meta = {
    description = "xonsh readable traceback";
    homepage = "https://github.com/vaaaaanquish/xontrib-readable-traceback";
    license = lib.licenses.mit;
  };
}
