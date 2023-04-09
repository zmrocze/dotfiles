{
  description = "A very basic flake";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    # nixpkgs.url = github:NixOS/nixpkgs;

  };

  outputs = { self, nixpkgs, flake-utils }: 
    flake-utils.lib.eachSystem [ "x86_64-linux" ] 
      (system: rec {

        docker-example = (system: 
            let 
              pkgsLinux = import nixpkgs { inherit system ; };
            in nixpkgs.dockerTools.buildImage {
                name = "with-zsh";
                config = {
                  Cmd = [ "${pkgsLinux.zsh}/bin/hello" ];
                };
              }
          );

        packages = {
            docker-zsh = docker-example {inherit nixpkgs; };
        };
      )
}
