{
  # package helvum

  config = {

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
    # sound.enable = true;
    security.rtkit.enable = true;

    hardware = {
      pulseaudio.enable = false;
      bluetooth.enable = true;

    };
  };
}
