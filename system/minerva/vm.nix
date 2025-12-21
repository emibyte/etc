{pkgs, ...}: {
  # virt-manager
  environment.systemPackages = with pkgs; [
    win-spice
    virtio-win
    dnsmasq
  ];
  programs.virt-manager.enable = true;
  users.groups.libvirtd.members = ["emily"];
  virtualisation.spiceUSBRedirection.enable = true;
  virtualisation.libvirtd = {
    enable = true;
    qemu = {
      package = pkgs.qemu_kvm;
      runAsRoot = true;
      swtpm.enable = true;
    };
  };

  # virtual box
  # virtualisation.virtualbox.host.enable = true;
  # users.extraGroups.vboxusers.members = [ "emily" ];
  # virtualisation.virtualbox.host.enableExtensionPack = true;
}
