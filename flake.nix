{
  description = "Custom Emacs build with Nix-managed packages, inspired by Neovim setup";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };
  outputs =
    inputs@{
      self,
      nixpkgs,
      flake-parts,
      ...
    }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "aarch64-darwin"
        "aarch64-linux"
        "x86_64-darwin"
        "x86_64-linux"
      ];
      perSystem =
        {
          config,
          pkgs,
          system,
          ...
        }:
        let
          inherit (pkgs) alejandra just mkShell;

          myEmacsPackages =
            epkgs: with epkgs; [
              vterm
              magit
              org
              which-key
              use-package
              evil
              evil-collection
              projectile
              company
              catppuccin-theme
              # Add more packages as needed
            ];

          myEmacs = (pkgs.emacsPackagesFor pkgs.emacs).emacsWithPackages myEmacsPackages;

          mkEmacsPlugin =
            {
              name ? "user",
              src ? ./.,
            }:
            pkgs.stdenv.mkDerivation {
              inherit name src;
              buildPhase = "true";
              installPhase = ''
                mkdir -p $out/share/emacs/site-lisp
                cp -r . $out/share/emacs/site-lisp/${name}
              '';
            };

          userEmacsPlugin = mkEmacsPlugin {
            name = "user";
            src = ./.;
          };

        in
        {
          packages = {
            default = pkgs.symlinkJoin {
              name = "custom-emacs";
              paths = [
                myEmacs
                userEmacsPlugin
              ];
              buildInputs = [ pkgs.makeWrapper ];
              postBuild = ''
                wrapProgram $out/bin/emacs \
                  --add-flags "-Q" \
                  --add-flags "-l $out/share/emacs/site-lisp/user/init.el"
              '';
              meta = myEmacs.meta // {
                platforms = pkgs.emacs.meta.platforms;
              };
            };
          };

          apps.default = {
            type = "app";
            program = "${config.packages.default}/bin/emacs";
          };

          devShells.default = mkShell { buildInputs = [ just ]; };

          formatter = alejandra;
        };
    };
}
