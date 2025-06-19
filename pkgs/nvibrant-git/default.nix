{
  git,
  python3Packages,
  fetchgit,
}:
python3Packages.buildPythonApplication rec {
  pname = "nvibrant-git";
  version = "1.0.5";
  pyproject = true;

  src = fetchgit {
    url = "https://github.com/Tremeschin/nVibrant";
    rev = "v${version}";
    deepClone = true;
    leaveDotGit = true;
    fetchSubmodules = true;
    hash = "sha256-aOhzunEsQFMhLw5QqxLIYr1oquM+6zsUW8rV7DsSG9o=";
  };

  nativeBuildInputs = [
    python3Packages.meson
    python3Packages.ninja
    python3Packages.hatchling
    git
  ];

  propagatedBuildInputs = with python3Packages; [packaging];

  patchVariables = ''
    from nvibrant import NVIBRANT\
    NINJA=(\"ninja\",)\
    RESOURCES=(NVIBRANT/\"resources\")
  '';

  postPatch = ''
    # Patch the build.py file to use the correct relative paths
    mv nvibrant/build.py ./

    # Patch the build.py variables to call Ninja directly
    # and move the resources folder
    sed -i '/def build() -> None:/i ${patchVariables}' build.py
  '';

  preBuild = ''
    # Generate the executables for the driver versions
    python build.py
  '';

  postInstall = ''
    libdir=$out/lib/${python3Packages.python.libPrefix}/site-packages

    # Move the resources folder to the libdir to be accessible
    # by the nvibrant wrapper
    cp -vr resources $libdir/nvibrant
  '';
}
