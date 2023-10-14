let chs = import ./_channels.nix {};
    path = chs.master;
    pkgs = import path {};
in with pkgs;
  mkShell rec {
    name = "GHC-HEAD";
    buildInputs = [
      haskell.compiler.ghcHEAD
      cabal-install
      stack
    ];
    shellHook = ''
      export PS1="=>(${name})$PS1"
    '';
  }
