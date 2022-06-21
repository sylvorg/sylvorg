{ buildPythonPackage
, fetchFromGitHub
, pythonOlder
, poetry-core
, addict
, autoslot
, click
, coconut
, cytoolz
, hy
, hyrule
, more-itertools
, nixpkgs
, rich
, toolz
}:

buildPythonPackage rec {
  pname = "oreo";
  version = "1.0.0.0";
  format = "pyproject";
  disabled = pythonOlder "3.9";

  src = fetchFromGitHub {
    owner = "syvlorg";
    repo = "oreo";
    rev = "6cb1453b0d13613ca1d19bd1b419b107b99a2f6d";
    sha256 = (builtins.fromJSON ''{
    "url": "https://github.com/syvlorg/oreo",
    "rev": "6cb1453b0d13613ca1d19bd1b419b107b99a2f6d",
    "date": "2022-06-06T01:44:35+00:00",
    "path": "/nix/store/3l0ylwn5f4kyrv31q6h9kzgrpw7h8if6-oreo-6cb1453",
    "sha256": "0f9a43r65gzrfhllgwzga6yz4ws0jcsqhfvjmq3bda11hadrrs5f",
    "fetchLFS": false,
    "fetchSubmodules": true,
    "deepClone": false,
    "leaveDotGit": false
  }
  '').sha256;
  };

  buildInputs = [ poetry-core ];
  nativeBuildInputs = buildInputs;

  propagatedBuildInputs = [
      addict
      autoslot
      click
      coconut
      cytoolz
      hy
      hyrule
      more-itertools
      nixpkgs
      rich
      toolz
  ];

  pythonImportsCheck = [ "oreo" ];

  postPatch = ''
    substituteInPlace pyproject.toml --replace "rich = { git = \"https://github.com/syvlorg/rich.git\", branch = \"master\" }" ""
    substituteInPlace setup.py --replace "'rich @ git+https://github.com/syvlorg/rich.git@master'," ""
  '';

  meta = {
    description = "The Stuffing for Other Functions!";
    homepage = "https://github.com/syvlorg/oreo";
  };
}
