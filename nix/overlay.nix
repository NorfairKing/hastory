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
        "hastory-data"
        "hastory-data-cli"
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
              self: super: final.hastoryPackages
            );
        }
    );
}
