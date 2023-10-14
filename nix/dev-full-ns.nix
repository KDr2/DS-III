let chs = import ./_channels.nix {};
    path = chs.ustc-unstable;
    pkgs = import path {};
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
