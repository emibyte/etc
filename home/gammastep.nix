{...}: {
  services.gammastep = {
    enable = true;

    # TODO: still need to add geoclue2 service in configuration.nix
    provider = "geoclue2";
  };
}
