{pkgs, ...}: {
  fonts.packages = with pkgs;
    [
      iosevka-comfy.comfy
      noto-fonts
      inter
      maple-mono.NF-unhinted
      maple-mono.truetype
      font-awesome
    ]
    ++ builtins.filter lib.attrsets.isDerivation (builtins.attrValues pkgs.nerd-fonts);
}
