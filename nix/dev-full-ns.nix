let
 pkgs = import path {};
 path = <ustc-unstable>;
 dev-pkgs = import ./dev-full-pl.nix;
in
pkgs.mkShell rec {
  name = "NIX DEV FULL";
  packages = dev-pkgs;
  buildInputs = [];
  shellHook = ''
  export PS1="=>(${name})$PS1"
  '';
}
