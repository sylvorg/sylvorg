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
    rev = "master";
    sha256 = "1dds9dwf5bqxi84s1fzcdykiqgcc1iq3rh6p76wjz6h7cb451h08";
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
