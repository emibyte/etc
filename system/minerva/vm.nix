{pkgs, ...}: {
  # virt-manager
  environment.systemPackages = with pkgs; [virtio-win];
  programs.virt-manager.enable = true;
  users.groups.libvirtd.members = ["emily"];
  virtualisation.libvirtd.enable = true;
  virtualisation.spiceUSBRedirection.enable = true;
}
