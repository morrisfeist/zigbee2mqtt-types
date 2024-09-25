{
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
    };
}
