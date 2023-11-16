/*
We need NIXPKGS_ALLOW_UNFREE=1 because of:
- google-chrome-dev
*/
let chs = import ./_channels.nix {};
    path = chs.unstable;
    pkgs = import path {};
in with pkgs;
  mkShell rec {
    name = "Virtual Desktop";
    packages = [
      # * Basics
      glibcLocales
      # * X11, WM
      tigervnc
      icewm
      # * Fonts and Icons
      noto-fonts-emoji
      arc-icon-theme
      # * Applications
      lxterminal
      evince
      google-chrome
      xchm
      # nix-daemon needs a proxy to install this in China Mainland:
      # chromiumDev
      # * Developing libraries
      gtk3
    ];
    buildInputs = [];
    LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
    shellHook = ''
      export PS1="=> (${name})$PS1"
      export LC_ALL=en_US.UTF-8
    '';
  }
