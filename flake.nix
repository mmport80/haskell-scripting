{
  description = "Haskell scripting";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      systems = [ "aarch64-darwin"  ];
      forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f system);
    in
    {
      devShells = forAllSystems (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          ghc = pkgs.haskellPackages.ghcWithPackages (p: [
            p.text
            p.directory
            p.process
            p.time
          ]);
        in
        {
          default = pkgs.mkShell {
            buildInputs = [ ghc ];
          };
        }
      );
    };
}
