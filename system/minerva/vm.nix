{...}: {
  programs.virt-manager.enable = true;

  users.groups.libvirtd.members = ["emily"];

  virtualisation.libvirtd.enable = true;

  virtualisation.spiceUSBRedirection.enable = true;
}
