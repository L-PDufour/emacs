{
  description = "Custom Emacs build with multiple configuration files";

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

        myInitEl = pkgs.writeText "init.el" ''
          ;; Main initialization file
          (load "~/.emacs.d/config.el")
          (load "~/.emacs.d/packages.el")
          ;; Add more loads if needed
        '';

        myConfigEl = pkgs.writeText "config.el" ''
          ;; Your general Emacs configuration
          (setq inhibit-startup-screen t)
          (setq initial-scratch-message "")
          (menu-bar-mode -1)
          (tool-bar-mode -1)
          (scroll-bar-mode -1)
          (column-number-mode t)
          ;; Add more configuration here
        '';

        myPackagesEl = pkgs.writeText "packages.el" ''
          ;; Package-specific configuration
          (require 'package)
          (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
          (package-initialize)
          ;; Configure your packages here
        '';

        myEmacs = pkgs.emacsWithPackages (epkgs: [
          epkgs.vterm
          epkgs.magit
          epkgs.org
          epkgs.which-key
          # Add more packages here
        ]);

      in
      {
        packages.default = myEmacs.overrideAttrs (old: {
          postInstall = ''
            ${old.postInstall or ""}
            mkdir -p $out/share/emacs/site-lisp
            cp ${myInitEl} $out/share/emacs/site-lisp/init.el
            cp ${myConfigEl} $out/share/emacs/site-lisp/config.el
            cp ${myPackagesEl} $out/share/emacs/site-lisp/packages.el
          '';
        });

        apps.default = flake-utils.lib.mkApp { drv = self.packages.${system}.default; };
      }
    );
}
