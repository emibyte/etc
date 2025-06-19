{pkgs, ...}: {
  # NOTE: mainly for running veadotube-mini
  home.packages = with pkgs; [
    (let
      base = pkgs.appimageTools.defaultFhsEnvArgs;
    in
      pkgs.buildFHSEnv (base
        // {
          name = "fhs";
          targetPkgs = pkgs:
            (base.targetPkgs pkgs)
            ++ (
              with pkgs; [
                harfbuzz
                freetype
                icu
              ]
            );
          profile = "export FHS=1";
          runScript = "bash";
          extraOutputsToInstall = ["dev"];
        }))
  ];
}
