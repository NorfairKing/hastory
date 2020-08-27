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

                  # YamlParse-Applicative
                  yamlparseApplicativeRepo =
                    final.fetchFromGitHub {
                      owner = "NorfairKing";
                      repo = "yamlparse-applicative";
                      rev = "1d381a4cbc9736a2defc916a93cfcf8000ee7e37";
                      sha256 =
                        "sha256:18arsg3qzva8hz4f78a3n5zp639pway90xlvwac67fgv4sl6ivaz";
                    };
                  yamlparseApplicativePkg =
                    name:
                      dontCheck (
                        self.callCabal2nix name ( yamlparseApplicativeRepo + "/${name}" ) {}
                      );
                  yamlparseApplicativePackages =
                    final.lib.genAttrs [
                      "yamlparse-applicative"
                    ] yamlparseApplicativePkg;
                  # envparse
                  envparseRepo =
                    final.fetchFromGitHub {
                      owner = "supki";
                      repo = "envparse";
                      rev = "de5944fb09e9d941fafa35c0f05446af348e7b4d";
                      sha256 =
                        "sha256:0piljyzplj3bjylnxqfl4zpc3vc88i9fjhsj06bk7xj48dv3jg3b";
                    };
                  envparsePkg =
                    dontCheck (
                      self.callCabal2nix "envparse" ( envparseRepo ) {}
                    );
                in
                  final.hastoryPackages // {
                    # Passwords
                    ghc-byteorder = self.callHackage "ghc-byteorder" "4.11.0.0.10" {};
                    envparse = envparsePkg;
                  } // passwordPackages // yamlparseApplicativePackages
            );
        }
    );
}
