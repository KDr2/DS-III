let pkgs = import <nixpkgs> {};
    unstable-pkgs = import <nixpkgs-unstable> {};
in with pkgs;
  mkShell rec {
    name="dummy";
    buildInputs= [
    ];
    shellHook = ''
      export PS1="=> (${name})$PS1"
    '';
  }
