{ nixpkgs ? <nixpkgs>
}:
let
  inherit (import nixpkgs {}) pkgs;
  p = pkgs.haskellPackages;
in
  pkgs.haskellPackages.shellFor {
    packages = _:[ (import ./. {}).orgr ];
  }
