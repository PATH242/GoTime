with (import (builtins.fetchTarball {
  url = "https://github.com/dmjio/miso/archive/refs/tags/1.8.3.tar.gz";
  /* sha256 = "1yb9yvc0ln4yn1jk2k5kwwa1s32310abawz40yd8cqqkm1z7w6wg"; */
}) { });
let
  codeworld-api = pkgs.haskell.packages.ghcjs.callCabal2nix "codeworld-api" (builtins.fetchTarball {
    url = "https://hackage.haskell.org/package/codeworld-api-0.8.1/codeworld-api-0.8.1.tar.gz";
  }) {
    codeworld-game-api = codeworld-game-api;
    codeworld-prediction = codeworld-prediction;
    reflex = reflex;
  };
  codeworld-game-api = pkgs.fetchFromGitHub {
    owner = "google";
    repo = "codeworld";
    rev = "4d33407969e9373832fd4e38878e4ff470038e49";
    sha256 = "1c8xyplp4wfymaggk0drjwwnjka9pzsydhaq3y6ywa0ddrbzd50f";
  } + "/codeworld-game-api";
  codeworld-prediction = pkgs.fetchFromGitHub {
    owner = "google";
    repo = "codeworld";
    rev = "4d33407969e9373832fd4e38878e4ff470038e49";
    sha256 = "1c8xyplp4wfymaggk0drjwwnjka9pzsydhaq3y6ywa0ddrbzd50f";
  } + "/codeworld-prediction";
  reflex = pkgs.haskell.packages.ghcjs.callCabal2nix "reflex" (builtins.fetchTarball {
    url = "https://hackage.haskell.org/package/reflex-0.9.0.1/reflex-0.9.0.1.tar.gz";
  }) {
    commutative-semigroups = pkgs.haskell.packages.ghcjs.callCabal2nix "commutative-semigroups" (builtins.fetchTarball {
      url = "https://hackage.haskell.org/package/commutative-semigroups-0.1.0.1/commutative-semigroups-0.1.0.1.tar.gz";
    }) {};
    semialign = pkgs.haskell.packages.ghcjs.callCabal2nix "semialign" (builtins.fetchTarball {
      url = "https://hackage.haskell.org/package/semialign-1.1.0.1/semialign-1.1.0.1.tar.gz";
    }) {
      these = pkgs.haskell.packages.ghcjs.callCabal2nix "these" (builtins.fetchTarball {
        url = "https://hackage.haskell.org/package/these-1.1.1.1/these-1.1.1.1.tar.gz";
      }) {};
    };
    these = pkgs.haskell.packages.ghcjs.callCabal2nix "these" (builtins.fetchTarball {
      url = "https://hackage.haskell.org/package/these-1.1.1.1/these-1.1.1.1.tar.gz";
    }) {};
    these-lens = pkgs.haskell.packages.ghcjs.callCabal2nix "these-lens" (builtins.fetchTarball {
      url = "https://hackage.haskell.org/package/these-lens-1.0.1.3/these-lens-1.0.1.3.tar.gz";
    }) {};
    constraints-extras = constraints-extras;
    dependent-map = pkgs.haskell.packages.ghcjs.callCabal2nix "dependent-map" (builtins.fetchTarball {
      url = "https://hackage.haskell.org/package/dependent-map-0.4.0.0/dependent-map-0.4.0.0.tar.gz";
    }) {
      constraints-extras = constraints-extras;
    };
    patch = pkgs.haskell.packages.ghcjs.callCabal2nix "patch" (builtins.fetchTarball {
      url = "https://hackage.haskell.org/package/patch-0.0.5.2/patch-0.0.5.2.tar.gz";
    }) {
      constraints-extras = constraints-extras;
      semialign = semialign;
    };
    witherable = pkgs.haskell.packages.ghcjs.callCabal2nix "witherable" (builtins.fetchTarball {
      url = "https://hackage.haskell.org/package/witherable-0.3.5/witherable-0.3.5.tar.gz";
    }) {};
  };
  constraints-extras = pkgs.haskell.packages.ghcjs.callCabal2nix "constraints-extras" (builtins.fetchTarball {
      url = "https://hackage.haskell.org/package/constraints-extras-0.3.2.1/constraints-extras-0.3.2.1.tar.gz";
    }) {};
  semialign = pkgs.haskell.packages.ghcjs.callCabal2nix "semialign" (builtins.fetchTarball {
    url = "https://hackage.haskell.org/package/semialign-1.1.0.1/semialign-1.1.0.1.tar.gz";
  }) {
    these = pkgs.haskell.packages.ghcjs.callCabal2nix "these" (builtins.fetchTarball {
      url = "https://hackage.haskell.org/package/these-1.1.1.1/these-1.1.1.1.tar.gz";
    }) {};
  };
in
  pkgs.haskell.packages.ghcjs.callCabal2nix "codeworld-template" ./. {
    codeworld-api = codeworld-api;
  }
