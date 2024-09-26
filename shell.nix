{ pkgs ? import <nixpkgs> { } }:

let
  haskellShell =
    let
      haskellPackages = pkgs.haskellPackages.extend
        (pkgs.haskell.lib.compose.packageSourceOverrides {
          zigbee2mqtt-types = ./.;
        });
    in
    haskellPackages.shellFor {
      packages = p: [ p.zigbee2mqtt-types ];
      withHoogle = true;
      nativeBuildInputs = with haskellPackages; [
        cabal-install
        haskell-language-server
        ghcid
      ];
    };

  nixShell = pkgs.mkShell {
    buildInputs = with pkgs; [
      nixd
      nixpkgs-fmt
    ];
  };

  nodeShell = pkgs.mkShell {
    buildInputs = with pkgs; [
      nodejs_22
    ];
  };
in
pkgs.mkShell { inputsFrom = [ haskellShell nixShell nodeShell ]; }
