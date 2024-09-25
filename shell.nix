{ pkgs ? import <nixpkgs> { } }:

let
  nixShell = pkgs.mkShell {
    buildInputs = with pkgs; [
      nixd
      nixpkgs-fmt
    ];
  };

  nodeShell = pkgs.mkShell {
    buildInputs = with pkgs; [ nodejs_22 ];
  };
in
pkgs.mkShell { inputsFrom = [ nixShell nodeShell ]; }
