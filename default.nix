let

  name = "generic-spot-the-differences";
  compiler-nix-name = "ghc883";

  sources = import ./nix/sources.nix {};
  haskellNix = import (fetchTarball { inherit (sources."haskell.nix") url sha256; }) {};
  all-hies = import (fetchTarball { inherit (sources.all-hies) url sha256; }) {};

  pkgs =
    (import haskellNix.sources.nixpkgs-2003 (haskellNix.nixpkgsArgs // {
      overlays = haskellNix.nixpkgsArgs.overlays ++ [
        all-hies.overlay
      ];
    }));

  set = pkgs.haskell-nix.cabalProject' {

    inherit compiler-nix-name;

    src = pkgs.haskell-nix.haskellLib.cleanGit {
      inherit name;
      src = ./.;
    };

  };

  fromNiv = pkg: pkgs.haskellPackages.callCabal2nix pkg (pkgs.fetchzip { inherit (sources.${pkg}) url sha256; }) {};
  dontCheck = pkgs.haskell.lib.dontCheck;

in

set.hsPkgs.${name}.components.library // {

  shell = set.hsPkgs.shellFor {

    buildInputs = [
      # add development packages here
    ];

    exactDeps = true;

    packages = p: [
      p.generic-spot-the-differences
    ];

    shellHook = ''
       export HIE_HOOGLE_DATABASE=$(realpath "$(dirname "$(realpath "$(which hoogle)")")/../share/doc/hoogle/default.hoo")
     '';

    tools = {
      cabal = "3.2.0.0";
      hie = "unstable";
      hlint = "2.2.11";
      hpack = "0.34.2";
    };

  };

}
