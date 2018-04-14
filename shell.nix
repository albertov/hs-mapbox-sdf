{ nixpkgs ? <nixpkgs>
}:
let pkgs = import nixpkgs {};
in (pkgs.haskellPackages.callPackage ./. {}).env
