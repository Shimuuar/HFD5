let
  pkgs     = import <nixpkgs> {inherit overlays config;};
  config   = {};
  overlays = [];
in
pkgs.mkShell {
  buildInputs = [
    pkgs.hdf5
    pkgs.ghc
    pkgs.pkg-config
  ];
  shellHook = ''
    export LD_LIBRARY_PATH=${pkgs.hdf5}/lib''${LD_LIBRARY_PATH:+:}$LD_LIBRARY_PATH
    taskset -pc 0-1000 $$
    '';
}
