{}:
{
  master = fetchTarball https://github.com/NixOS/nixpkgs/archive/master.tar.gz;
  ustc-unstable = fetchTarball https://mirrors.ustc.edu.cn/nix-channels/nixpkgs-unstable/nixexprs.tar.xz;
  v2305 = fetchTarball https://github.com/NixOS/nixpkgs/archive/refs/tags/23.05.tar.gz;
}
