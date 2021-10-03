# -*- compile-command: "nix-shell --run 'cabal exec -- ghc-pkg list'"; -*-
# -*- compile-command: "direnv exec . bash -c 'pip list; cabal exec -- ghc-pkg list'"; -*-
{ pkgs ? import (import ./nix/sources.nix {}).nixpkgs {}, sources ? import ./nix/sources.nix {} }:
  # https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/haskell-modules/make-package-set.nix
let mach-nix = import sources.mach-nix {};

    pyEnv = mach-nix.mkPython {
      # _.tomli.buildInputs.add = [pkgs.python310Packages.flit-core];
      # _.distlib.buildInputs.add = [pkgs.python310Packages.flit-core];
      # # _.jedi.buildInputs.add = [pkgs.python310Packages.platformdirs];
      # _.black.buildInputs.add = [pkgs.python310Packages.platformdirs];
      # _.black.propagatedBuildInputs.mod = pySelf: _: old:
        # old ++ [pkgs.python310Packages.platformdirs];
      # providers =  {default = "conda,wheel,sdist";
      #               tomli = "conda,wheel,sdist";
                    # };
      requirements = ''
        pandas
        cbor2
        ipython
        stringcase
      '';
      # probagatedBuildInputs = [pkgs.pkg-config];
    };

    minimalCompilation = x: with pkgs.haskell.lib; disableLibraryProfiling (dontHaddock x);
    minSource = self: name: minimalCompilation (self.callCabal2nix name sources.${name} {});

    this = with pkgs.haskell.lib; pkgs.haskellPackages.developPackage {
      root = ./.;
      withHoogle = false;
      returnShellEnv = false;
      overrides = self: super:  {
        yahp              = minSource self "yahp";
        chronos           = minSource self "chronos";
        interval          = minSource self "interval";
        hoff              = minSource self "hoff";
        date-combinators  = minSource self "date-combinators";
        cache-polysemy   = doJailbreak super.cache-polysemy;
      };
      source-overrides = {
        vector-algorithms = "0.9.0.1";
        ghc-tcplugin-api  = "0.8.3.0";
        polysemy          = "1.9.0.0";
        resource-pool     = "0.4.0.0";
      };
      modifier = with pkgs.haskell.lib; drv:
        disableLibraryProfiling (dontHaddock (addBuildTools drv
          (with pkgs.haskellPackages; [ cabal-install ghcid pyEnv
                                      ])));
    };
in this
   // { env = this.env.overrideAttrs(_: prev: { shellHook = prev.shellHook + ''

   ''; });}
