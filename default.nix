{ nixpkgs ? <nixpkgs>
}:
let
    inherit (import nixpkgs {}) pkgs;
in {
  orgr = pkgs.haskellPackages.callCabal2nix "orgr" (pkgs.lib.cleanSource ./.) {};
}
    
