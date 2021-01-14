{
  mkInstance = { marloweDash }:

    { config, pkgs, lib, ... }:
    {
      imports = [ (defaultMachine pkgs) ];

      networking.firewall = {
        enable = true;
        allowedTCPPorts = [ 80 ];
      };

      systemd.services.marlowe-dash = {
        wantedBy = [ ];
        before = [ ];
        enable = true;
        path = [ "${marloweDash}" ];

        serviceConfig = {
          TimeoutStartSec = "0";
          Restart = "always";
          DynamicUser = true;
          ProtectKernelTunables = true;
          ProtectControlGroups = true;
          ProtectKernelModules = true;
          PrivateDevices = true;
          SystemCallArchitectures = "native";
          CapabilityBoundingSet = "~CAP_SYS_ADMIN";
          AmbientCapabilities = [ "CAP_NET_BIND_SERVICE" ];
        };

        script = "marlowe-dashboard-server webserver -b 0.0.0.0 -p 80";
      };

    };
}
