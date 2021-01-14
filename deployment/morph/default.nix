let
  plutus = import ../../. { };
  machines = (plutus.pkgs.lib.importJSON ./machines.json);
  overlays = import ./overlays.nix;
  stdOverlays = [ overlays.journalbeat ];
  nixpkgsLocation = https://github.com/NixOS/nixpkgs/archive/5272327b81ed355bbed5659b8d303cf2979b6953.tar.gz;
  options = { inherit stdOverlays machines nixpkgsLocation; };
  defaultMachine = (import ./default-machine.nix) options;
  marloweDashMachine = import ./marlowe-dash.nix;
in
{
  machines.marlowe-dash-a.ip = marloweDashMachine.mkInstance { inherit defaultMachine; };
}
