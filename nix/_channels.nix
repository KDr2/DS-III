/*

Nix already has system level channels which can be manipulated by the
`nix-channel` command. But to make nix-shell independent to these
system level channels, I collect some usefull channel here.

But I failed to find a way to customize substituter locally, so if you
want a substituter, you should set it in the /etc/nix/nix.conf file:

substituters = https://mirror.sjtu.edu.cn/nix-channels/store/

*/

{}:
rec {
  master = fetchTarball https://github.com/NixOS/nixpkgs/archive/master.tar.gz;
  unstable = ustc-unstable;
  v2305 = fetchTarball https://github.com/NixOS/nixpkgs/archive/refs/tags/23.05.tar.gz;
  # mirror
  official-unstable = fetchTarball https://nixos.org/channels/nixpkgs-unstable/nixexprs.tar.xz;
  ustc-unstable = fetchTarball https://mirrors.ustc.edu.cn/nix-channels/nixpkgs-unstable/nixexprs.tar.xz;
}
