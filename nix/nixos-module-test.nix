{ pkgs ? import ./pkgs.nix }:
let
  hastory-production =
    import ( ./nixos-module.nix ) { envname = "production"; };
  home-manager =
    import (
      builtins.fetchTarball {
      url = "https://github.com/rycee/home-manager/archive/472ca211cac604efdf621337067a237be9df389e.tar.gz";
      sha256 = "sha256:1gbfsnd7zsxwqryxd4r6jz9sgdz6ghlkapws1cdxshrbxlwhqad1";
    } + "/nixos/default.nix"
    );

  port = 8001;
in
  pkgs.nixosTest (
    { lib, pkgs, ... }:
      {
        name = "hastory-module-test";
        machine =
          {
            imports =
              [
                hastory-production
                home-manager
              ];
            services.hastory.production =
              {
                enable = true;
                inherit port;
              };
            users.users.testuser.isNormalUser = true;
            home-manager.users.testuser =
              { pkgs, ... }:
                {
                  imports =
                    [
                      ./home-manager-module.nix
                    ];
                  xdg.enable = true;
                  home.stateVersion = "20.09";
                  programs.hastory =
                    {
                      enable = true;
                      sync =
                        {
                          enable = true;
                          server-url = "localhost:${builtins.toString port}";
                          username = "testuser";
                          password = "testpass";
                        };
                    };
                };
          };
        testScript =
          ''
      from shlex import quote

      machine.start()
      machine.wait_for_unit("multi-user.target")

      machine.wait_for_open_port(${builtins.toString port})
      machine.succeed("curl localhost:${builtins.toString port}")

      machine.wait_for_unit("home-manager-testuser.service")


      def su(user, cmd):
          return f"su - {user} -c {quote(cmd)}"


      machine.succeed(su("testuser", "cat ~/.config/hastory/config.yaml"))

      # Sync
      machine.succeed(su("testuser", "hastory register"))
      machine.succeed(su("testuser", "hastory login"))
      machine.succeed(su("testuser", "hastory sync"))
    '';
      }
  )
