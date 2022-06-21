{ lib
, buildPythonPackage
, fetchFromGitHub
, pytestCheckHook
, colorama
}:

buildPythonPackage rec {
  pname = "backtrace";
  version = "0.2.1";

  src = fetchFromGitHub {
    owner = "nir0s";
    repo = pname;
    rev = "a1f75c956f669a6175088693802d5392e6bd7e51";
    sha256 = "sha256-F4tvYQ9XmKALMiwak+oa7nqWoLe2zvVOiTv9/gmQfcQ=";
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
