let

  name = "generic-spot-the-differences";
  compiler-nix-name = "ghc883";
  fromNiv = niv: fetchTarball { inherit (sources.${niv}) url sha256; };

  sources = import ./nix/sources.nix {};
  haskellNix = import (fromNiv "haskell.nix") {
      sourcesOverride = {
        hackageSrc = fromNiv "hackage.nix";
      };
  };
  all-hies = import (fromNiv "all-hies") {};

  pkgs = import haskellNix.sources.nixpkgs-2003 (haskellNix.nixpkgsArgs // {
    overlays = haskellNix.nixpkgsArgs.overlays ++ [
      all-hies.overlay
    ];
  });

  set = pkgs.haskell-nix.cabalProject {

    inherit compiler-nix-name;

    src = pkgs.haskell-nix.haskellLib.cleanGit {
      inherit name;
      src = ./.;
    };

  };

in

set.${name}.components.library // {

  shell = set.shellFor {

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
