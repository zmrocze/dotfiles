{ pkgs, ... }:

{
  # !!! Requires patched `sedutil`, see ./pkgs.

  # Note: From recollection: it seems like using instructions from link at:
  # https://nixos.wiki/wiki/Self_Encryption_Drive
  # i put "linux pba" on my drive (onetime thing), and instructed the drive 
  # with sedutil commands to use OPAL 2 hardware encryption (onetime thing).
  # The code here and the patch is only a thing that saves cleartext password in ram before s3 sleep.
  # The pr with the patch is unmerged after 8 years, update to nixpkgs-25.05 fails.
  # Reverting to use of sedutil from nixpkgs-24.11.

  # # NOTE: Generate the password hash with: sudo sedutil-cli --printPasswordHash 'plaintext-password-here' /dev/nvme0n1  
  systemd.services.sedutil-s3sleep = {
    description = "Enable S3 sleep on OPAL self-encrypting drives";
    documentation =
      [ "https://github.com/Drive-Trust-Alliance/sedutil/pull/190" ];
    path = [ pkgs.sedutil ];
    script =
      "sedutil-cli -n -x --prepareForS3Sleep 0 5edc5ff23532f918310961b6cd0ee1d474f76e1ca08cbbf8b5533c2deab91b78 /dev/nvme0n1";
    wantedBy = [ "multi-user.target" ];
  };

  # Sleep
  #   https://www.kernel.org/doc/html/latest/admin-guide/pm/sleep-states.html#basic-sysfs-interfaces-for-system-suspend-and-hibernation
  #   Force hybrid-sleep on suspend:
  #     - When suspending, write RAM to disk (hibernate)
  #     - When done writing hibernation image, suspend.
  environment.etc."systemd/sleep.conf".text = ''
    [Sleep]
    SuspendState=disk
    SuspendMode=suspend
  '';

}
