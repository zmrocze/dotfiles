{
  description = "A very basic flake";

  inputs = {
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  }

  outputs = { self, nixpkgs }: let
      systems = ["x86_64-linux"]
      perSystem = nixpkgs.lib.genAttrs systems
      in {

        packages.x86_64-linux.hello = nixpkgs.legacyPackages.x86_64-linux.hello;

        packages.x86_64-linux.default = self.packages.x86_64-linux.hello;



        docker-example = { 
            pkgs ? nixpkgs
          , pkgsLinux ? import nixpkgs { inherit system ; }
          }:

            pkgs.dockerTools.buildImage {
            name = "with-zsh";
            config = {
              Cmd = [ "${pkgsLinux.zsh}/bin/hello" ];
            };
          }  

        apps = perSystem {

        }
    };
}
