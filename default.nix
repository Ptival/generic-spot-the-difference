let

  name = "generic-spot-the-differences";
  compiler-nix-name = "ghc883";

  sources = import ./nix/sources.nix {};

  haskellNix = import (fetchTarball { inherit (sources."haskell.nix") url sha256; }) {};

  all-hies = fetchTarball {
    url = "https://github.com/infinisil/all-hies/tarball/09ba836904fa290b5e37d5403150ea0c921661fb";
    sha256 = "sha256:0qbjqv1fkhkx1cffqybz1mfks1jphh0vh3zd8ad2qd6lch4gyys4";
  };

  pkgs =
    (import haskellNix.sources.nixpkgs-2003 (haskellNix.nixpkgsArgs // {
      overlays = haskellNix.nixpkgsArgs.overlays ++ [
        (import all-hies {}).overlay
      ];
    }));

  set = pkgs.haskell-nix.cabalProject' {

    inherit compiler-nix-name;

    modules = [{
      # Make Cabal reinstallable
      nonReinstallablePkgs = [

        "Win32"
        "array"
        "array"
        "base"
        "binary"
        "bytestring"
        "containers"
        "deepseq"
        "directory"
        "filepath"
        "ghc"
        "ghc-boot"
        "ghc-boot"
        "ghc-boot-th"
        "ghc-compact"
        "ghc-heap"
        "ghc-prim"
        "ghc-prim"
        "ghcjs-prim"
        "ghcjs-th"
        "hpc"
        "integer-gmp"
        "integer-simple"
        "mtl"
        "parsec"
        "pretty"
        "process"
        "rts"
        "template-haskell"
        "terminfo"
        "text"
        "time"
        "transformers"
        "unix"
        "xhtml"

      ];
    }];

    pkg-def-extras = [
      (hackage: {
        packages =
          let
          in
            {

              # "Cabal" = hackage.Cabal."3.2.0.0".revisions.default;
              # floskell = hackage.floskell."0.10.3".revisions.default;

              # Cabal = selfHaskell.callHackage "Cabal" "3.2.0.0" {};
              # brittany = dontCheck (selfHaskell.callHackage "brittany" "0.12.1.1" {});
              # butcher = dontCheck (selfHaskell.callHackage "butcher" "1.3.3.2" {});
              # extra = dontCheck (selfHaskell.callHackage "extra" "1.7.4" {});
              # floskell = dontCheck (selfHaskell.callHackage "floskell" "0.10.3" {});
              # ghc-check = selfHaskell.callHackage "ghc-check" "0.5.0.1" {};
              # ghc-exactprint = selfHaskell.callHackage "ghc-exactprint" "0.6.2" {};
              # ghc-lib-parser = selfHaskell.callHackage "ghc-lib-parser" "8.10.1.20200523" {};
              # ghcide = dontCheck (fromNiv "ghcide");
              # haskell-language-server = dontCheck (fromNiv "haskell-language-server");
              # haskell-lsp = selfHaskell.callHackage "haskell-lsp" "0.22.0.0" {};
              # haskell-lsp-types = selfHaskell.callHackage "haskell-lsp-types" "0.22.0.0" {};
              # hie-bios = dontCheck (selfHaskell.callHackage "hie-bios" "0.6.1" {});
              # hslogger = selfHaskell.callHackage "hslogger" "1.3.1.0" {};
              # lsp-test = dontCheck (selfHaskell.callHackage "lsp-test" "0.11.0.2" {});
              # network = dontCheck (selfHaskell.callHackage "network" "2.8.0.1" {});
              # opentelemetry = selfHaskell.callHackage "opentelemetry" "0.4.2" {};
              # ormolu = selfHaskell.callHackage "ormolu" "0.1.2.0" {};
              # parser-combinators = selfHaskell.callHackage "parser-combinators" "1.2.1" {};
              # stylish-haskell = selfHaskell.callHackage "stylish-haskell" "0.11.0.0" {};

            };
      })
    ];

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
