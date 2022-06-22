{ buildPythonPackage
, fetchFromGitHub
, pythonOlder
, poetry-core
, oreo
}:

buildPythonPackage rec {
  pname = "bakery";
  version = "2.0.0.0";
  format = "pyproject";
  disabled = pythonOlder "3.9";

  src = fetchFromGitHub {
    owner = "syvlorg";
    repo = pname;
    rev = "116ef86d11a0f77ed96e4c3ecd1a4f33bbddd965";
    sha256 = "0q4sh1cyrwx6swbka04nb7bwwba3k9jvycpsrqm288rhhj4bhhyg";
  };

  buildInputs = [ poetry-core ];
  nativeBuildInputs = buildInputs;

  propagatedBuildInputs = [ oreo ];

  pythonImportsCheck = [ "bakery" ];

  postPatch = ''
    substituteInPlace pyproject.toml --replace "oreo = { git = \"https://github.com/syvlorg/oreo.git\", branch = \"main\" }" ""
    substituteInPlace setup.py --replace "'oreo @ git+https://github.com/syvlorg/oreo.git@main'" ""
  '';

  meta = {
    homepage = "https://github.com/syvlorg/bakery";
  };
}
