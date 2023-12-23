{ username, lib, ... }: {

  options = {
    username = lib.mkOption { type = lib.types.str; };
    hostname = lib.mkOption { type = lib.types.str; };
  };

}
