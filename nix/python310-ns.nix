let pkgs = import path {};
    path = <ustc-unstable>;
in with pkgs;
  mkShell rec {
    name = "Python310";
    packages = [
      python310Full
      python310Packages.virtualenv
      python310Packages.ipython
      # python310Packages.numpy
    ];
    buildInputs = [];
    shellHook = ''
      export PS1="=>(${name})$PS1"
    '';
  }
