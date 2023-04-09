{
  description = "A very basic flake";


  outputs = { self, nixpkgs }: {

    packages.x86_64-linux.hello = nixpkgs.legacyPackages.x86_64-linux.hello;

    packages.x86_64-linux.default = self.packages.x86_64-linux.hello;

    docker-example = { 
        pkgs ? nixpkgs
      , pkgsLinux ? import nixpkgs { inherit system ; }
      }:

      pkgs.dockerTools.buildImage {
      name = "hello-docker";
      config = {
        Cmd = [ "${pkgsLinux.hello}/bin/hello" ];
      };
      }
    };  
}
