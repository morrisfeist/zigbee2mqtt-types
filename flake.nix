{
  description = "Haskell type definitions for devices supported by zigbee2mqtt";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  };

  outputs = { nixpkgs, ... }:
    let
      allSystems = builtins.attrNames nixpkgs.legacyPackages;
      forAllSystems = f: nixpkgs.lib.genAttrs allSystems
        (system: f (import nixpkgs { inherit system; }));
    in
    {
      devShells = forAllSystems (pkgs: { default = import ./shell.nix { inherit pkgs; }; });

      packages = forAllSystems (pkgs: rec {
        default = zigbee2mqtt-types-source;

        zigbee2mqtt-types-source =
          let
            packageYaml = builtins.path { path = ./package.yaml; };
            hsSrc = builtins.path { path = ./src; };
          in
          pkgs.buildNpmPackage {
            pname = "zigbee2mqtt-types-source";
            version = "0.0.1";
            src = ./generator;
            nodejs = pkgs.nodejs_22;
            npmDepsHash = "sha256-XFECsHO2pfjxGLv+iAluKCRwIFRFZovqmuB5tJ3V3z4=";
            installPhase = ''
              mkdir -p $out
              npm start -- $out ${packageYaml}

              mkdir -p $out/src
              cp -r ${hsSrc}/* $out/src
            '';
          };

        lib = pkgs.haskellPackages.callCabal2nix "zigbee2mqtt-types" zigbee2mqtt-types-source {
          # Dependency overrides go here
        };


      });
    };
}
