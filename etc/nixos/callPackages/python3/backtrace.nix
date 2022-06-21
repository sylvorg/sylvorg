{ lib
, buildPythonPackage
, fetchFromGitHub
, pytestCheckHook
, colorama
}:

buildPythonPackage rec {
  pname = "backtrace";
  version = "0.2.1";

  src = fetchTarball {
    url = "https://github.com/nir0s/${pname}/archive/a1f75c956f669a6175088693802d5392e6bd7e51.tar.gz";
    sha256 = "1i3xj04zxz9vi57gbkmnnyh9cypf3bm966ic685s162p1xhnz2qp";
  };

  propagatedBuildInputs = [ colorama ];

  checkInputs = [ pytestCheckHook ];

  pythonImportsCheck = [ "backtrace" ];

  meta = {
    description = "Makes Python tracebacks human friendly";
    homepage = "https://github.com/nir0s/backtrace";
    license = lib.licenses.asl20;
  };
}
