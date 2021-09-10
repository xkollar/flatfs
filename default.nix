{ pkgs ? import <nixpkgs> {} }:

let
  skipList = [ "disk.img" ];
  skipFilter = name: type:
    ! builtins.elem (baseNameOf (toString name)) skipList;
  cleanSkip = src: pkgs.lib.cleanSourceWith { filter = skipFilter; inherit src; };
  clean = pkgs.lib.cleanSource (cleanSkip ./.);
  base = pkgs.haskellPackages.callCabal2nix "flatfs" clean {};
  flatfs = base.overrideAttrs (
    old: {
      # nativeBuildInputs = old.nativeBuildInputs ++ [
      #   pkgs.util-linux
      # ];
    }
  );
in
  flatfs
