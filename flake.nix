{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils }: flake-utils.lib.eachDefaultSystem (system:
    let pkgs = import nixpkgs { 
      inherit system; 
      overlays = [
        (self: super: {
	  haskell = super.haskell // {
	    packages = super.haskell.packages // {
	      ghc923 = super.haskell.packages.ghc923.override {
	        overrides = new: old: {
	          mutable-containers = super.haskell.lib.dontCheck old.mutable-containers;
		};
	      };
	    };
	  };
	})
      ];
    };
    in rec {
      devShell = pkgs.mkShell {
        buildInputs = with pkgs; [
          haskell.packages.ghc923.ghc
          cabal-install
          (haskell-language-server.override { supportedGhcVersions = [ "923" ]; })
	  pandoc
          cabal2nix
        ];
      };
      packages.little = pkgs.haskell.packages.ghc923.callPackage ./little.nix {};
      defaultPackage = packages.little;
  });
}
