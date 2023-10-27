let chs = import ./_channels.nix {};
    path = chs.v2305;
    pkgs = import path {};
    dev-pkgs = import ./dev-full-pl.nix;
in
pkgs.mkShell rec {
  name = "NIX DEV FULL";
  packages = dev-pkgs;
  buildInputs = [];
  shellHook = ''
  export PS1="=> (${name})$PS1"
  export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:${pkgs.postgresql.lib.outPath + "/lib"}
  '';
}
