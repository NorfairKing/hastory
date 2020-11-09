{ lib, pkgs, config, ... }:

with lib;

let
  cfg = config.programs.hastory;


in
{
  options =
    {
      programs.hastory =
        {
          enable = mkEnableOption "Sparep cli and syncing";
          extraConfig =
            mkOption {
              type = types.str;
              description = "Extra contents for the config file";
              default = "";
            };
          sync =
            mkOption {
              default = null;
              type =
                types.nullOr (
                  types.submodule {
                    options =
                      {
                        enable = mkEnableOption "Sparep syncing";
                        server-url =
                          mkOption {
                            type = types.str;
                            example = "api.hastory.cs-syd.eu";
                            description = "The url of the sync server";
                          };
                        username =
                          mkOption {
                            type = types.str;
                            example = "syd";
                            description =
                              "The username to use when logging into the sync server";
                          };
                        password =
                          mkOption {
                            type = types.str;
                            example = "hunter12";
                            description =
                              "The password to use when logging into the sync server";
                          };
                      };
                  }
                );
            };
        };
    };
  config =
    let
      hastoryPkgs = (import ./pkgs.nix).hastoryPackages;
      configContents =
        cfg:
          ''
        
${cfg.extraConfig}

      '';
      syncConfigContents =
        syncCfg:
          optionalString (syncCfg.enable or false) ''

server-url: "${cfg.sync.server-url}"
username: "${cfg.sync.username}"
password: "${cfg.sync.password}"

      '';


      syncSparepName = "sync-hastory";
      syncSparepService =
        {
          Unit =
            {
              Description = "Sync hastory";
              Wants = [ "network-online.target" ];
            };
          Service =
            {
              ExecStart =
                "${pkgs.writeShellScript "sync-hastory-service-ExecStart"
                  ''
                    exec ${hastoryPkgs.hastory-cli}/bin/hastory sync
                  ''}";
              Type = "oneshot";
            };
        };
      syncSparepTimer =
        {
          Unit =
            {
              Description = "Sync hastory every day";
            };
          Install =
            {
              WantedBy = [ "timers.target" ];
            };
          Timer =
            {
              OnCalendar = "daily";
              Persistent = true;
              Unit = "${syncSparepName}.service";
            };
        };

      hastoryConfigContents =
        concatStringsSep "\n" [
          (configContents cfg)
          (syncConfigContents cfg.sync)
        ];

      services =
        (
          optionalAttrs (cfg.sync.enable or false) {
            "${syncSparepName}" = syncSparepService;
          }
        );
      timers =
        (
          optionalAttrs (cfg.sync.enable or false) {
            "${syncSparepName}" = syncSparepTimer;
          }
        );
      packages =
        [
          hastoryPkgs.hastory-cli
        ];


    in
      mkIf cfg.enable {
        xdg =
          {
            configFile."hastory/config.yaml".text = hastoryConfigContents;
          };
        systemd.user =
          {
            startServices = true;
            services = services;
            timers = timers;
          };
        home.packages = packages;
      };
}
