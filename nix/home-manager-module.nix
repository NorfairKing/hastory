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
          enable = mkEnableOption "Hastory cli and syncing";
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
                        enable = mkEnableOption "Hastory syncing";
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

url: "${cfg.sync.server-url}"
username: "${cfg.sync.username}"
password: "${cfg.sync.password}"

      '';


      syncHastoryName = "sync-hastory";
      syncHastoryService =
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
      syncHastoryTimer =
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
              Unit = "${syncHastoryName}.service";
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
            "${syncHastoryName}" = syncHastoryService;
          }
        );
      timers =
        (
          optionalAttrs (cfg.sync.enable or false) {
            "${syncHastoryName}" = syncHastoryTimer;
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
