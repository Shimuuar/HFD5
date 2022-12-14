let
  pkgs     = import <nixpkgs> {inherit overlays config;};
  config   = {};
  overlays = [];
  pyp = pkgs.python3.withPackages (ps: with ps;
    [ ipython
      h5py
    ]);
in
pkgs.mkShell {
  buildInputs = [
    pkgs.hdf5
    pkgs.ghc
    pkgs.haskellPackages.cabal-install
    pkgs.pkg-config
    #
    pyp
  ];
  shellHook = ''
    export LD_LIBRARY_PATH=${pkgs.hdf5}/lib''${LD_LIBRARY_PATH:+:}$LD_LIBRARY_PATH
    taskset -pc 0-1000 $$
    '';
}
