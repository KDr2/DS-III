let pkgs = import path {};
    path = chs.ustc-unstable;
    chs = import ./_channels.nix {};
in with pkgs;
  mkShell rec {
    name = "Julia-1.8";
    packages = [
      julia_18
    ];
    buildInputs = [];
    shellHook = ''
      export PS1="=>(${name})$PS1"
      export JULIA_PKG_SERVER=https://mirrors.ustc.edu.cn/julia
    '';
  }
