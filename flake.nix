{
  description = "hsttp";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = nixpkgs.legacyPackages.${system};

      hsttp =
        pkgs.haskellPackages.callCabal2nix "hsttp" self {};
    in {
      packages = {
        inherit hsttp;
        default = hsttp;
      };

      devShell = pkgs.mkShell {
        buildInputs = with pkgs; [
          file
          haskellPackages.haskell-language-server
          haskellPackages.ghcid
          haskellPackages.cabal-install
          hsttp
        ];
      };
    });
}
