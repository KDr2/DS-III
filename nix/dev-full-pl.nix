#
# use
#   `nix-env -f norm.nix -i '.*'`
# to install all packages in this file
#
let chs = import ./_channels.nix {};
    path = chs.ustc-unstable;
    # path = <ustc-unstable>;
    pkgs = import path {};
in
with pkgs;
[
  # * Basics
  glibcLocales
  # * X11, Graphics
  tigervnc
  fluxbox
  gtk3
  # * Python
  python310Full
  python310Packages.virtualenv
  python310Packages.ipython
  # * Nodejs and WASM
  nodejs
  emscripten
  # * C/C++
  clang_16
  gcc
  # * Rust
  rustc
  rustup
  cargo
  # * Haskell
  ghc
  cabal-install
  stack
  # agda
  # * Lisp
  clojure
  leiningen
  babashka
  racket
  chez
  sbcl
  # * Julia
  julia-bin
  # * R
  R
  # * Emacs
  emacs
  # * Others
  perl
  perl538Packages.Appcpanminus
]
