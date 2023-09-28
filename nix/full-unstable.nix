let pkgs = import path {};
    path = <ustc-unstable>;
in with pkgs;
  mkShell rec {
    name = "NIX UNSTABLE FULL";
    packages = [
      # X11
      tigervnc
      fluxbox
      # python
      python310Full
      python310Packages.virtualenv
      # Nodejs and WASM
      nodejs
      emscripten
      # C/C++
      clang_16
      gcc
      # Rust
      rustc
      rustup
      cargo
      # Haskell
      ghc
      # Lisp
      clojure
      leiningen
      babashka
      racket
      chez
      sbcl
      # Julia
      julia-bin
      # R
      R
      # Emacs
      emacs
      # GTK
      gtk3
      # others
      perl
      perl538Packages.Appcpanminus
    ];
    buildInputs = [];
    shellHook = ''
      export PS1="=>(${name})$PS1"
    '';
  }
