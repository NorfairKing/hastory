let
  pkgs = import ./nix/pkgs.nix;
  pre-commit-hooks = import ./nix/pre-commit.nix;
in
pkgs.mkShell {
  name = "smos-nix-shell";
  buildInputs = pre-commit-hooks.tools;
  shellHook = ''
    ${pre-commit-hooks.run.shellHook}


    function nix-build_ {
      nix-build \
        --option extra-substituters https://iohk.cachix.org \
        --option trusted-public-keys iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= \
        --option extra-substituters https://validity.cachix.org \
        --option trusted-public-keys validity.cachix.org-1:CqZp6vt9ir3yB5f8GAtfkJxPZG8hKC5fhIdaQsf7eZE= \
        --option extra-substituters https://yamlparse.cachix.org \
        --option trusted-public-keys yamlparse.cachix.org-1:DLkIYUWCK4HdTen7mwYsf2LB8o+REcV73MONfnAtQsY= \
        --option extra-substituters https://smos.cachix.org \
        --option trusted-public-keys smos.cachix.org-1:YOs/tLEliRoyhx7PnNw36cw2Zvbw5R0ASZaUlpUv+yM= \
        $*
    }
    alias nix-build=nix-build_
  '';
}
