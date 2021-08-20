let nix = import ./nix { };
in
{
  inherit (nix.pkgs.haskellPackages) orgr;
}
