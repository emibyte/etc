{...}: {
  # virt-manager
  #environment.systemPackages = with pkgs; [virtio-win];
  #programs.virt-manager.enable = true;
  #users.groups.libvirtd.members = ["emily"];
  #virtualisation.libvirtd.enable = true;
  #virtualisation.spiceUSBRedirection.enable = true;

  # virtualbox
  virtualisation.virtualbox.host.enable = true;
  virtualisation.virtualbox.host.enableExtensionPack = true;
  users.extraGroups.vboxusers.members = ["emily"];
}
