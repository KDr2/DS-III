let chs = import ./_channels.nix {};
    path = chs.ustc-unstable;
    # path = <ustc-unstable>;
    pkgs = import path {};
in with pkgs;
  mkShell rec {
    name = "Scala3";
    packages = [
      scala_3
      scala-cli
      sbt
    ];
    buildInputs = [];
    shellHook = ''
      export PS1="=> (${name})$PS1"
    '';
  }
