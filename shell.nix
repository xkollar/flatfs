{ pkgs ? import <nixpkgs> {} }:
let
 drv = (import ./. {});
 drvWithTools = drv.env.overrideAttrs (
    old: with pkgs.haskellPackages; {
      nativeBuildInputs = old.nativeBuildInputs ++ [
        ghcid
        cabal-install
        hlint
      ];
      shellHook = "hpack";
    }
  );
in
  drvWithTools
