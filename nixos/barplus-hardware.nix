# Do not modify this file!  It was generated by ‘nixos-generate-config’
# and may be overwritten by future invocations.  Please make changes
# to /etc/nixos/configuration.nix instead.
{ lib, modulesPath, ... }:

{
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
    (modulesPath + "/profiles/all-hardware.nix")
  ];

  # https://www.reddit.com/r/archlinux/comments/117z37j/linux_62_fails_to_boot_on_my_f2fs_root/
  # TODO: check if needed
  boot = {
    kernelParams = [ "rw" ];
    initrd.kernelModules = [ ];
    kernelModules = [ "kvm-intel" ];
    extraModulePackages = [ ];
  };

  fileSystems = {
    "/" = {
      device =
        "/dev/disk/by-label/BAR_NIXOS"; # also "/dev/disk/by-label/BAR_NIXOS"
      fsType = "f2fs";
    };

    "/boot" = {
      device =
        "/dev/disk/by-label/BAR_BOOT"; # also "/dev/disk/by-label/BAR_BOOT"
      fsType = "vfat";
    };

    "/mnt/bar_exfat" = {
      device =
        "/dev/disk/by-label/BAR_EXFAT"; # also "/dev/disk/by-label/BAR_EXFAT"
      fsType = "exfat";
    };
  };

  swapDevices = [ ];

  # Enables DHCP on each ethernet and wireless interface. In case of scripted networking
  # (the default) this is the recommended approach. When using systemd-networkd it's
  # still possible to use this option, but it's recommended to use it in conjunction
  # with explicit per-interface declarations with `networking.interfaces.<interface>.useDHCP`.
  networking.useDHCP = lib.mkDefault true;
  # networking.interfaces.docker0.useDHCP = lib.mkDefault true;
  # networking.interfaces.eno1.useDHCP = lib.mkDefault true;
  # networking.interfaces.wlo1.useDHCP = lib.mkDefault true;

  # defined in hosts/barplus.nix
  # nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  hardware.cpu.intel.updateMicrocode = true;
  hardware.cpu.amd.updateMicrocode = true;
}
