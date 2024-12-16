# cheatsheet:
# 
# # The /dev/*midi* devices are just for compatibility with the legacy OSS interface; the actual ALSA devices are in /dev/snd/.
# # To show incoming MIDI messages, run `aseqdump -p xxx` with the port name shown by `aseqdump -l`.
# 

{inputs}: {config, lib, ... }: with lib; with types; {
  # package helvum

  imports = [
    inputs.musnix.nixosModules.musnix
  ];

  options.my-sound = {
    enable = mkEnableOption "My sound configuration: pipewire + realtime";
    useMusenix = mkEnableOption "Use musenix (for realtime + vst envs)";
  };

  config = mkIf config.my-sound.enable (mkMerge [
    {
      services.pipewire = {
        enable = true;
        alsa.enable = true;
        alsa.support32Bit = true;
        pulse.enable = true;
        # If you want to use JACK applications, uncomment this
        jack.enable = true;

        # use some session manager, wireplumber is default
        wireplumber.enable = true;
      };

      # needed pre kernel 6.7
      environment.etc = {
        "modprobe.d/scarlett.conf".text = ''
          options snd_usb_audio vid=0x1235 pid=0x8204 device_setup=1
        '';
      };

      hardware = {
        pulseaudio.enable = false;
        bluetooth.enable = true;
      };
    }
    (mkIf (! config.my-sound.useMusenix ) {
      security.rtkit.enable = true;
      users.users.${config.username}.extraGroups = [ "rtkit" ];
    })
    (mkIf config.my-sound.useMusenix {
      musnix.enable = true;
      # users.users.${config.username}.extraGroups = [ "audio" ]; # already set
    })
  ]);
}
