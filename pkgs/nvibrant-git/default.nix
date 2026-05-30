{
  stdenv,
  fetchFromGitHub,
  meson,
  ninja,
  pkg-config,
}: let
  openGpu = fetchFromGitHub {
    owner = "NVIDIA";
    repo = "open-gpu-kernel-modules";
    rev = "595.71.05";
    hash = "sha256-Lfz71QWKM6x/jD2B22SWpUi7/og30HRlXg1kL3EWzEw=";
  };
in
  stdenv.mkDerivation (finalAttrs: {
    pname = "nvibrant";
    version = "1.2.1";

    src = fetchFromGitHub {
      owner = "Tremeschin";
      repo = "nVibrant";
      rev = "v${finalAttrs.version}";
      fetchSubmodules = false;
      hash = "sha256-pEOZQjfAlNWUEbrfFEuPAaabWjilvMaAjpdJedNPDs0=";
    };

    nativeBuildInputs = [meson ninja pkg-config];

    postPatch = ''
      rm -rf open-gpu
      cp -r ${openGpu} open-gpu
      chmod -R u+w open-gpu
    '';

    mesonBuildType = "release";

    installPhase = ''
      runHook preInstall
      install -Dm755 nvibrant $out/bin/nvibrant
      runHook postInstall
    '';
  })
