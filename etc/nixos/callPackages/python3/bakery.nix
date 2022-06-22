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
    rev = "4e0ca9be0d8004c2c2db7d92bb669fcb2d8ba904";
    sha256 = "145lx81fgnjflw9j6w60ddr77p5sq3az8c4x4i2f8507yriamn3c";
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
