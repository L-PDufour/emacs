{ pkgs }:

repoName: repoUrl: rev:
pkgs.stdenv.mkDerivation {
  name = repoName;
  src = pkgs.fetchgit {
    url = repoUrl;
    inherit rev;
    sha256 = "kKkOSqLONQIciiu4gElRMZBjEz7OqDuQB7cgs6VNw1E=";
  };
  buildPhase = "true"; # No build needed
  installPhase = ''
    mkdir -p $out/share/emacs/site-lisp/${repoName}
    cp -r . $out/share/emacs/site-lisp/${repoName}
  '';
}
