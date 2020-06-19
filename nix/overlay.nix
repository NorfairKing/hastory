final: previous:
with final.haskell.lib;

{
  hastoryPackages =
    let
      hastoryPkg =
        name:
          doBenchmark (
            addBuildDepend (
              failOnAllWarnings (
                disableLibraryProfiling (
                  final.haskellPackages.callCabal2nix name ( final.gitignoreSource ( ../. + "/${name}" ) ) {}
                )
              )
            ) ( final.haskellPackages.autoexporter )
          );
    in
      final.lib.genAttrs [
        "hastory-cli"
        "hastory-cli-data"
        "hastory-data"
        "hastory-data-gen"
        "hastory-path"
        "hastory-server"
        "hastory-server-data"
      ] hastoryPkg;
  haskellPackages =
    previous.haskellPackages.override (
      old:
        {
          overrides =
            final.lib.composeExtensions (
              old.overrides or (_:
            _:
              {})
            ) (
              self: super:
                let
                  # Passwords
                  passwordRepo =
                    final.fetchFromGitHub {
                      owner = "cdepillabout";
                      repo = "password";
                      rev = "26434d4f6888faf8dc36425b20b59f0b5056d7f5";
                      sha256 =
                        "sha256:0kbrw7zcn687h61h574z5k8p7z671whblcrmd6q21gsa2pyrk4ll";
                    };
                  passwordPkg =
                    name:
                      dontCheck (
                        self.callCabal2nix name ( passwordRepo + "/${name}" ) {}
                      );
                  passwordPackages =
                    final.lib.genAttrs [
                      "password"
                      "password-instances"
                    ] passwordPkg;
                in
                  final.hastoryPackages // {
                    # Passwords
                    ghc-byteorder = self.callHackage "ghc-byteorder" "4.11.0.0.10" {};
                  } // passwordPackages
            );
        }
    );
}
