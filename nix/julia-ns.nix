let chs = import ./_channels.nix {};
    path = chs.unstable;
    pkgs = import path {};
in with pkgs;
  mkShell rec {
    name = "Julia, the latest release.";
    packages = [
      julia
    ];
    buildInputs = [];
    shellHook = ''
      export PS1="=> (${name})$PS1"
      export JULIA_PKG_SERVER=https://mirrors.ustc.edu.cn/julia
    '';
  }
