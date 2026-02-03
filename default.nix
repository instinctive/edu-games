let
  pkgs = import <nixpkgs> {};
  haskellEnv = pkgs.haskellPackages.developPackage {
    root = ./.;
    modifier = drv:
      pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages; [
        cabal-install
        ghcid
        hpack
        markdown-unlit
      ]);
    returnShellEnv = true;
  };
in
  haskellEnv.overrideAttrs (oldAttrs: {
    shellHook = (oldAttrs.shellHook or "") + ''
    '';
  })
