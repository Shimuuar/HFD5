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
    taskset -pc 0-1000 $$
    '';
}
