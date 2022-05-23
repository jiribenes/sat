{ sources ? import ./nix/sources.nix, nixpkgs ? sources.nixpkgs, compiler ? "ghc8107" }:
let
  pkgs = import nixpkgs { inherit config; };

  # use gitignore to scout which files are important and which aren't
  gitIgnore = pkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];

  config = {
    allowBroken = true;
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages."${compiler}".override {
            overrides = haskellPackagesNew: haskellPackagesOld: rec {
              # our package:  
              sat = haskellPackagesNew.callCabal2nix "sat" (gitIgnore ./.) { };
            };
          };
        };
      };
    };
  };
  myHaskellPackages = pkgs.haskell.packages.${compiler};

  # development nix-shell
  shell = with myHaskellPackages; shellFor {
    packages = p: [
      p.sat
    ];

    buildInputs = [
      # !!! Add developer tools here: !!!
      # cabal
      cabal-install
      cabal2nix

      # dhall (config language)
      pkgs.dhall

      # IDE-related
      haskell-language-server
      hlint
      # stan

      # formatters
      pkgs.nixpkgs-fmt

      # Niv for pinning dependencies
      pkgs.niv
      # (import sources.niv { }).niv
    ];

    # enable hoogle
    withHoogle = true;
  };

  # static executable
  static-exe = pkgs.haskell.lib.justStaticExecutables (myHaskellPackages.sat);
in
{
  inherit shell static-exe;

  # non-static build
  sat = myHaskellPackages.sat;
}
