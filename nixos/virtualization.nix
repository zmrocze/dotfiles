{ config, ... }: {
  # imports = [];
  # options = {};

  config = {
    virtualisation.libvirtd.enable = true;
    programs.virt-manager.enable = true;

    users.users.${config.username}.extraGroups = [ "libvirtd" ];

    boot = {
      initrd.availableKernelModules = [
        "kvm_intel"
        # "kvm_amd"
      ];
      kernelModules = [ "kvm-intel" ];
    };
  };
}
