{
  description = "envelopes";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          ghc = pkgs.ghc;
        in
        {
          devShells.default = import ./shell.nix { inherit pkgs ghc; };
        }
      );
}
