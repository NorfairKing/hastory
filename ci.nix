let
  pkgs = import ./nix/pkgs.nix;
  pre-commit-hooks = import ./nix/pre-commit.nix;
in
pkgs.hastoryPackages // {
  pre-commit-hooks = pre-commit-hooks.run;
}
