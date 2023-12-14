let chs = import ./_channels.nix {};
    path = chs.unstable;
    pkgs = import path {};
in with pkgs;
  mkShell rec {
    name = "Typography";
    packages = [
      pandoc
      typst
      texlive.combined.scheme-full
      gtk4
      # harfbuzz
      arcanPackages.harfbuzz
    ];
    buildInputs = [];
    shellHook = ''
      export PS1="=> (${name})$PS1"
    '';
  }
