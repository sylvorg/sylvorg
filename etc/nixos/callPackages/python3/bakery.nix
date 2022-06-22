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
    rev = "45cb6fd6153f60cacd00860c042c4517bfe2681e";
    sha256 = "067d61nn6s766p9icca5zs35429v8rbb6zkww6pab1wf9q084g7i";
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
