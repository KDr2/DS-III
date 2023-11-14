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
  export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:${pkgs.stdenv.cc.cc.lib}/lib
  export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:${pkgs.lib.makeLibraryPath packages}
  '';
}


  /*
  There are many ways to get the library path of a package:
  - pkgs.postgresql.lib.outPath + "/lib"
  - pkgs.lib.makeLibraryPath [pkgs...]
  */
