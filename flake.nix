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
        pkgs = import nixpkgs { inherit system; };

        myEmacsPackages =
          epkgs: with epkgs; [
            vterm
            magit
            org
            which-key
            use-package
            command-log-mode
            evil
            evil-collection
            evil-escape
            ligature
            projectile
            company
            catppuccin-theme
            vertico
            marginalia
            orderless
            consult
            embark
            embark-consult
            wgrep
          ];

        myEmacs = (pkgs.emacsPackagesFor pkgs.emacs).emacsWithPackages myEmacsPackages;

        emacsConfig = pkgs.runCommand "emacs-config" { } ''
          mkdir -p $out/share/emacs/site-lisp/user
          cp ${./init.el} $out/share/emacs/site-lisp/user/init.el
          cp ${./early-init.el} $out/share/emacs/site-lisp/user/early-init.el
          ${pkgs.lib.optionalString (builtins.pathExists ./extras) "cp -r ${./extras} $out/share/emacs/site-lisp/user/extras"}
        '';

      in
      {
        packages = {
          default = pkgs.symlinkJoin {
            name = "custom-emacs";
            paths = [
              myEmacs
              emacsConfig
            ];
            buildInputs = [ pkgs.makeWrapper ];
            postBuild = ''
              wrapProgram $out/bin/emacs \
                --add-flags "-Q" \
                --add-flags "-l $out/share/emacs/site-lisp/user/early-init.el" \
                --add-flags "-l $out/share/emacs/site-lisp/user/init.el" \
                --prefix PATH : ${
                  pkgs.lib.makeBinPath [
                    pkgs.ripgrep
                    pkgs.fd
                  ]
                }
            '';
          };
        };

        apps.default = {
          type = "app";
          program = "${self.packages.${system}.default}/bin/emacs";
        };

        devShells.default = pkgs.mkShell {
          buildInputs = [
            self.packages.${system}.default
            pkgs.just
          ];
        };
      }
    );
}
