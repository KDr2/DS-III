/*
We need NIXPKGS_ALLOW_UNFREE=1 because of:
- google-chrome-dev
*/
let chs = import ./_channels.nix {};
    path = chs.ustc-unstable;
    pkgs = import path {};
in with pkgs;
  mkShell rec {
    name = "Virtual Desktop";
    packages = [
      # * X11, Graphics
      noto-fonts-emoji
      tigervnc
      icewm
      gtk3
      google-chrome-dev
      # nix-daemon needs a proxy to install this in China Mainland:
      # chromiumDev
    ];
    buildInputs = [];
    shellHook = ''
      export PS1="=>(${name})$PS1"
    '';
  }
