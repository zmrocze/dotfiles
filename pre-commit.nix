{ inputs, ... }: {
  imports = [
    inputs.pre-commit-hooks.flakeModule # Adds perSystem.pre-commit options
  ];
  perSystem = { config, ... }: {
    devShells.pre-commit = config.pre-commit.devShell;
    pre-commit.settings = rec {
      excludes = [ "junk" "home-manager/gnome/dconf.nix" ];
      hooks = {
        nixfmt.enable = true;
        statix = { enable = true; };
        deadnix.enable = true;
        shellcheck.enable = true;
      };
      settings.statix.ignore = excludes;
    };
  };
}
