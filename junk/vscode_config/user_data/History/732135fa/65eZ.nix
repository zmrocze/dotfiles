{
  description = "A very basic flake";

  inputs = {
    # nixpkgs.url = github:NixOS/nixpkgs;

  }

  outputs = { self, nixpkgs }: let
      systems = ["x86_64-linux"]
      perSystem = nixpkgs.lib.genAttrs systems
      in {

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

        packages = perSystem (system: {
            docker-zsh = docker-example {inherit nixpkgs; };
          })  
    };
}
