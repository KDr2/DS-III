/* For locale problems, see https://nixos.wiki/wiki/Locales. */

let chs = import ./_channels.nix {};
    path = chs.v2305;
    pkgs = import path {};
    dev-pkgs = import ./dev-full-pl.nix;
in
pkgs.mkShell rec {
  name = "NIX DEV FULL";
  packages = dev-pkgs;
  buildInputs = [];
  LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
  shellHook = ''
  export PS1="=> (${name})$PS1"
  export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:${pkgs.postgresql.lib.outPath + "/lib"}:${pkgs.stdenv.cc.cc.lib}/lib
  '';
}
