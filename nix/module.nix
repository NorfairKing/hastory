{ envname }:
{ lib, pkgs, config, ... }:
with lib;

let
  cfg = config.services.hastory."${envname}";
  concatAttrs = attrList: fold ( x: y: x // y ) {} attrList;
in {
  options.services.hastory."${envname}" =
    {
      enable = mkEnableOption "Hastory Services";
      hosts =
        mkOption {
          type = types.listOf ( types.string );
          example = [ "hastory.example.com" ];
          description = "The host to serve web requests on";
        };
      port =
        mkOption {
          type = types.int;
          default = 8300;
          example = 8300;
          description = "The port to serve requests on";
        };
    };
  config =
    let
      workingDir = "/www/hastory/${envname}/data/";
      databaseFile = workingDir + "hastory.sqlite3";
      backupDir = workingDir + "backups/";
      webserver-service =
        let
          hastory-pkgs = (import ( ./pkgs.nix )).hastoryPackages;
          unlessNull = o: optionalAttrs ( !builtins.isNull o );
        in {
          description = "Hastory ${envname} Service";
          wantedBy = [ "multi-user.target" ];
          environment =
            concatAttrs [
            ];
          script =
            ''
              mkdir -p "${workingDir}"
              cd "${workingDir}"

              ${hastory-pkgs.hastory-server}/bin/hastory-server
            '';
          serviceConfig =
            {
              Restart = "always";
              RestartSec = 1;
              Nice = 15;
            };
          unitConfig =
            {
              StartLimitIntervalSec = 0;
              # ensure Restart=always is always honoured
            };
        };
    in
      mkIf cfg.enable {
        systemd.services =
          {
            "hastory-${envname}" = webserver-service;
          };
        networking.firewall.allowedTCPPorts = [ cfg.port ];
        services.nginx.virtualHosts =
          {
            "${head (cfg.hosts)}" =
              {
                enableACME = true;
                forceSSL = true;
                locations."/".proxyPass =
                  "http://localhost:${builtins.toString (cfg.port)}";
                serverAliases = tail cfg.hosts;
              };
          };
      };
}
