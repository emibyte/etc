{pkgs, ...}: {
  nvibrant-git = pkgs.callPackage ./nvibrant-git {};
  bootdev-cli = pkgs.callPackage ./bootdev-cli {};
}
