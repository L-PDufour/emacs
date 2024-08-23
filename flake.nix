{
  description = "Custom Emacs build with Nix-managed packages";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # Define the list of Emacs packages you want to use
        myEmacsPackages =
          epkgs: with epkgs; [
            vterm
            magit
            org
            which-key
            use-package
            evil
            projectile
            company
          ];

        # Create a custom Emacs with the specified packages
        myEmacs = pkgs.emacsWithPackages myEmacsPackages;

      in
      {
        packages.default = pkgs.symlinkJoin {
          name = "custom-emacs";
          paths = [ myEmacs ];
          buildInputs = [ pkgs.makeWrapper ];
          postBuild = ''
            mkdir -p $out/share/emacs/site-lisp
            cp ${./init.el} $out/share/emacs/site-lisp/init.el
            cp ${./config.el} $out/share/emacs/site-lisp/config.el
            wrapProgram $out/bin/emacs \
              --add-flags "-Q" \
              --add-flags "-l $out/share/emacs/site-lisp/init.el"
          '';
        };

        apps.default = flake-utils.lib.mkApp {
          drv = self.packages.${system}.default;
          name = "emacs";
        };
      }
    );
}
