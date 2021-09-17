{ pkgs ? import <nixpkgs> {} }:

let
  skipList = [ ".*\\.img" ];
  skipFilter = name: type:
    let base = (baseNameOf (toString name)); in
    ! builtins.any (skipPattern: builtins.isList (builtins.match skipPattern base)) skipList;
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
