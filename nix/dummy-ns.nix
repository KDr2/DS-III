let pkgs = import <nixpkgs> {};
in with pkgs;
  mkShell rec {
    name="dummy";
    buildInputs= [
    ];
    shellHook = ''
      export PS1="=> (${name})$PS1"
    '';
  }
