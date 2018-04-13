{ nixpkgs ? <nixpkgs>
}:
let pkgs = import nixpkgs {};
in pkgs.haskellPackages.ghcWithPackages (d: with d; [
  proto-lens
  proto-lens-protoc
])
