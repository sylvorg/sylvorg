with builtins; let
    pkgs = import (fetchGit { url = "https://github.com/shadowrylander/nixpkgs"; ref = "j"; }) {};
    hy = pkgs.python310Packages.hy.overridePythonAttrs (prev: {
      src = fetchGit { url = "https://github.com/scauligi/hy"; ref = "the_wheel_deal"; };
      # src = fetchGit { url = "https://github.com/hylang/hy"; ref = "master"; };
      postPatch = ''substituteInPlace setup.py --replace "\"funcparserlib ~= 1.0\"," ""'';
      disabledTests = [ "test_bin" "test_hy2py" ];
    });
    hyrule = pkgs.python310Packages.hyrule.overridePythonAttrs (prev: {
      src = fetchGit { url = "https://github.com/scauligi/hyrule"; ref = "precompile_hy"; };
      # src = fetchGit { url = "https://github.com/hylang/hyrule"; ref = "master"; };
      postPatch = ''substituteInPlace setup.py --replace "'hy @ git+https://github.com/scauligi/hy@the_wheel_deal#egg=hy'," ""'';
      propagatedBuildInputs = [ hy ];
      disabledTestPaths = [ "tests/test_slicing.hy" ];
    });
in with pkgs; mkShell {
    buildInputs = with python310Packages; [ (oreo.overridePythonAttrs (prev: {
        src = ./.;
        propagatedBuildInputs = (filter (p: ! elem p.pname [ "hy" "hyrule" ]) prev.propagatedBuildInputs) ++ [ hy hyrule ];
    })) ];
    # shellHook = "hy tests.hy; exit";
}
