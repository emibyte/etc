{
  config,
  pkgs,
  ...
}: let
  ghc = pkgs.haskellPackages.ghcWithPackages (hspkgs: [
    hspkgs.cabal-install
    hspkgs.safe
  ]);
in {
  home.packages = [
    # compilers
    ghc

    pkgs.jsonnet
    pkgs.nodejs
    # pkgs.python311Full
    pkgs.rustc
    pkgs.go
    pkgs.lua

    # language servers
    # NOTE: installing language servers globally instead of in a devshell is just simpler and doesnt lead to weird editor problems
    pkgs.haskell-language-server
    pkgs.jsonnet-language-server
    pkgs.lua-language-server
    pkgs.nixd
    pkgs.basedpyright
    pkgs.dockerfile-language-server
    pkgs.nodePackages."bash-language-server"
    pkgs.nodePackages."diagnostic-languageserver"
    pkgs.nodePackages."typescript"
    pkgs.nodePackages."typescript-language-server"
    pkgs.nodePackages."vscode-langservers-extracted"
    pkgs.nodePackages."yaml-language-server"
    pkgs.omnisharp-roslyn
    pkgs.gopls
    pkgs.rust-analyzer
    pkgs.terraform-ls

    # formatters
    pkgs.nixpkgs-fmt
    pkgs.gofumpt
    pkgs.golines
    # pkgs.python311Packages.black
    pkgs.rustfmt
    pkgs.terraform

    # tools
    pkgs.cargo
    pkgs.gcc
    pkgs.lazydocker
    pkgs.yarn
  ];
}
