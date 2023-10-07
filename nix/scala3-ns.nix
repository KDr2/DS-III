let pkgs = import path {};
    path = <ustc-unstable>;
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
      export PS1="=>(${name})$PS1"
    '';
  }
