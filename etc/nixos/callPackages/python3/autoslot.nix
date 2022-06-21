{ lib
, buildPythonPackage
, fetchFromGitHub
, pytestCheckHook
, flit
}:

buildPythonPackage rec {
  pname = "autoslot";
  version = "2021.10.1";
  format = "pyproject";

  src = fetchFromGitHub {
    owner = "cjrh";
    repo = pname;
    rev = "a36ea378136bc7dfdc11f3f950186f6ed8bee8c5";
    sha256 = "sha256-CMBQyGIHmi+5OdfAPHAMjD0cp2/su6AJih2v4nhLurU=";
  };

  buildInputs = [ flit ];
  nativeBuildInputs = buildInputs;

  checkInputs = [ pytestCheckHook ];

  pythonImportsCheck = [ "autoslot" ];

  meta = {
    description = "Automatic __slots__ for your Python classes";
    homepage = "https://github.com/cjrh/autoslot";
    license = lib.licenses.asl20;
  };
}
