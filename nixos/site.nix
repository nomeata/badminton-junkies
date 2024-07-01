{ config, lib, pkgs, environment, ihpApp, ... }:
let
  # TODO: Enable SSL/HTTPS when your domain records are hooked up
  # By enabling SSL, you accept the terms and conditions of LetsEncrypt
  httpsEnabled = true;
  jobsEnabled = false;
in
{
  services.cron = {
    enable = true;
    systemCronJobs = [
      # "*/30 * * * *      root    ${ihpApp}/bin/SomeScript"
    ];
  };

  security.acme.defaults.email = "mail@joachim-breitner.de";
  security.acme.acceptTerms = httpsEnabled;

  services.nginx = {
    enable = true;
    enableReload = true;
    recommendedProxySettings = true;
    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedTlsSettings = true;
  };
  services.nginx.virtualHosts = {
    # you can switch out "localhost" with a custom domain name
    "badjunk.nomeata.de" = {
      serverAliases = [ ];
      enableACME = httpsEnabled;
      forceSSL = httpsEnabled;
      locations = {
        "/" = {
          proxyPass = "http://localhost:8000";
          proxyWebsockets = true;
          extraConfig =
            # required when the target is also TLS server with multiple hosts
            "proxy_ssl_server_name on;" +
            # required when the server wants to use HTTP Authentication
            "proxy_pass_header Authorization;";
        };
      };
    };
  };

  services.postgresql = {
    enable = true;
    package = pkgs.postgresql_14;
    ensureDatabases = [ "defaultdb" ];
    ensureUsers = [
      {
        name = "shipadmin";
        # removed with IHP 1.3
        # ensurePermissions = {
        #   "DATABASE defaultdb" = "ALL PRIVILEGES";
        # };
      }
    ];
    # Set to true if you want to access your database from an external database manager like Beekeper Studio
    enableTCPIP = false;
    authentication = ''
      local all all trust
      host all all 127.0.0.1/32 trust
      host all all ::1/128 trust
      host all all 0.0.0.0/0 reject
    '';
  };

  # Enabling this automatically saves PostgresSQL dumps of your database to the file system
  services.postgresqlBackup.enable = false;
  # Path of directory where the PostgreSQL database dumps will be placed
  services.postgresqlBackup.location = "/var/backup/postgresql";
  services.postgresqlBackup.startAt = "*-*-* 01:15:00";

  systemd.services.ship = {
    description = "IHP service";
    enable = true;
    after = [
      "network.target"
      "postgresql.service"
    ];
    wantedBy = [
      "multi-user.target"
    ];
    serviceConfig = {
      Type = "simple";
      User = "ship";
      Restart = "always";
      WorkingDirectory = "${ihpApp}/lib";
      EnvironmentFile = /etc/shipnix/.env;
      ExecStart = "${ihpApp}/bin/RunProdServer";
    };
  };

  systemd.services.ship_jobs = {
    description = "IHP job watcher";
    enable = jobsEnabled;
    after = [ "ship.service" ];
    wantedBy = [
      "multi-user.target"
    ];
    serviceConfig = {
      Type = "simple";
      User = "ship";
      Restart = "always";
      WorkingDirectory = "${ihpApp}/lib";
      EnvironmentFile = /etc/shipnix/.env;
      ExecStart = '' ${ihpApp}/bin/RunJobs '';
    };
  };
}
