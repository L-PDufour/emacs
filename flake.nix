{
  description = "Custom Emacs build with Nix-managed packages";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      emacs-overlay,
      flake-utils,
      ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        overlays = [ emacs-overlay.overlay ];
        pkgs = import nixpkgs { inherit system overlays; };
        tangledConfig = ./emacs-config.org; # Ensure your Org file is in the same directory

        emacsWithPackages = pkgs.emacsWithPackagesFromUsePackage {
          config = tangledConfig;
          defaultInitFile = true;
          package = pkgs.emacs-unstable;
          alwaysEnsure = true;
          alwaysTangle = true;
          extraEmacsPackages = epkgs: [
            epkgs.vterm
          ];
        };
      in
      {
        packages.default = emacsWithPackages;
        apps.default = {
          type = "app";
          program = "${self.packages.${system}.default}/bin/emacs";
        };

        devShells.default = pkgs.mkShell {
          buildInputs = [
            self.packages.${system}.default
          ];
        };
      }
    );
}
