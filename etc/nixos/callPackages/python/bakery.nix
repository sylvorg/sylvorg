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
    rev = "9fadd8a2ec2f2e7ea9d2b0e8b9e378c34dfc21e8";
    sha256 = "0xlf3nw2kwyb1899rsbshixpb054l3gpw6xc50ccyjvcqcv28v1z";
  };

  buildInputs = [ poetry-core ];

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
