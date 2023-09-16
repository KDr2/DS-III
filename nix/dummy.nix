let pkgs = import <nixpkgs> {};
in with pkgs;
  mkShell rec {
    name="dummy";
    shellHook = ''
      export PS1="(${name})$PS1"
    '';
  }
