{ lib
, Python
, fetchFromGitHub
}:

Python.pkgs.buildPythonApplication rec {
  pname = "tailapi";
  version = "1.0.0.0";

  src = fetchFromGitHub {
    owner = "syvlorg";
    repo = pname;
    rev = "ada101b9c2919cfbe365c7d5fe71710d95425ca3";
    sha256 = "0bywlkbwxsnf11i1kv36ls2yax0pp30qmgcyfq3gcmzzhcgk2q3k";
  };

  propagatedBuildInputs = with Python.pkgs; [
    oreo
    requests
  ];

  installPhase = ''
    mkdir --parents $out/bin
    cp $src/${pname}.py $out/bin/${pname}
    chmod +x $out/bin/${pname}
  '';

  postFixup = "wrapProgram $out/bin/${pname} $makeWrapperArgs";

  makeWrapperArgs = [
    # "--prefix" "PATH" ":" (lib.makeBinPath [ git ])
    "--prefix PYTHONPATH : ${placeholder "out"}/lib/${Python.pkgs.python.libPrefix}/site-packages"
  ];

  meta.homepage = "https://github.com/syvlorg/${pname}";
}
