{
  description = "A very basic flake";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    # nixpkgs.url = github:NixOS/nixpkgs;

  };

  outputs = { self, nixpkgs, flake-utils }: 
    flake-utils.lib.eachSystem [ "x86_64-linux" ] 
      (system: let
          pkgs = import nixpkgs { inherit system ; };
          docker-example =
            pkgs.dockerTools.buildImage {
              name = "with-zsh";
              config = {
                Cmd = [ "${pkgs.zsh}/bin/zsh" ];
              };
              copyToRoot = with pkgs; [
                coreutils
                zsh
              ];
            };
        in {
          packages = {
            docker-zsh = docker-example;
          };
        }
    );
}
