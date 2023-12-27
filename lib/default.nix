{ inputs }: {
  pure = {
    hm-module-2-nixos-module = { hm-module, extraSpecialArgs }: {
      imports = [ inputs.home-manager.nixosModules.home-manager ];
      home-manager = {
        useGlobalPkgs = true;
        inherit extraSpecialArgs;
        users = { "${extraSpecialArgs.username}" = hm-module; };
      };
    };
  };
  overlay' = _: { };
}
