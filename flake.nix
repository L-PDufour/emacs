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
          inherit (pkgs)
            alejandra
            just
            mkShell
            lib
            ;

          fetchGitRepo = import ./fetch-git-repo.nix { inherit pkgs; };

          rpgtk =
            fetchGitRepo "rpgtk" "https://codeberg.org/howardabrams/emacs-rpgtk.git"
              "d7f6f53ecf1ea9eea5fb86faa46a1cd9420a10fe";

          myEmacsPackages =
            epkgs:
            (with epkgs; [
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
            ]);

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

          rpgtkPlugin = pkgs.stdenv.mkDerivation {
            name = "rpgtk";
            src = rpgtk;
            buildPhase = "true";
            installPhase = ''
              mkdir -p $out/share/emacs/site-lisp/rpgtk
              cp -r * $out/share/emacs/site-lisp/rpgtk/
            '';
          };
        in
        {
          packages = {
            default = pkgs.symlinkJoin {
              name = "custom-emacs";
              paths = [
                myEmacs
                userEmacsPlugin
                rpgtkPlugin
              ];
              buildInputs = [ pkgs.makeWrapper ];
              postBuild = ''
                wrapProgram $out/bin/emacs \
                  --set EMACSLOADPATH ${myEmacs}/share/emacs/site-lisp:${userEmacsPlugin}/share/emacs/site-lisp:${rpgtkPlugin}/share/emacs/site-lisp:${rpgtkPlugin}/share/emacs/site-lisp/rpgtk: \
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
