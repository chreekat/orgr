{ sources ? import ./sources.nix
}:
let
  pkgs = import sources.nixpkgs { overlays = [ projectOverlay ]; };

  # gitignore.nix 
  gitignoreSource = (import sources."gitignore.nix" { inherit (pkgs) lib; }).gitignoreSource;

  # pre-commit-hooks
  pre-commit-hooks = (import sources."pre-commit-hooks.nix");
  pre-commit-check = pre-commit-hooks.run {
    src = gitignoreSource ../.;
    hooks = {
      shellcheck.enable = true;
      nixpkgs-fmt.enable = true;
      nix-linter.enable = true;
      fourmolu.enable = true;
      hlint.enable = true;
      cabal-fmt.enable = true;
    };
  };

  projectOverlay = self: super: {
    haskellPackages = super.haskellPackages.extend
      (self.haskell.lib.packageSourceOverrides {
        orgr = gitignoreSource ../orgr;
      });
  };

  shell = pkgs.haskellPackages.shellFor {
    packages = p: [ p.orgr ];
    buildInputs = [ pkgs.cabal-install pkgs.niv ];
    shellHook = ''
      ${pre-commit-check.shellHook}
    '';
  };

in
{
  inherit pkgs shell;
}
