{pkgs, ...}:{
  programs.vesktop = {
    enable = true;
    package = pkgs.vesktop.overrideAttrs (old: {
      execWrapper = ''
        export NIXOS_OZONE_WL=1
        export ELECTRON_OZONE_PLATFROM_HINT=auto
        export __GLX_VENDOR_LIBRARY_NAME=nvidia
'';

      postFixup = ''
      wrapProgram $out/bin/vesktop \
        --add-flags "--enable-features=WaylandLinuxDrmSyncobj,VaapiVideoDecoder" \
        --add-flags "--disable-gpu-compositing" \
        --add-flags "--use-gl=egl";
'';
    });
  };
}
