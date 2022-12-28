{
  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs;
    rust-overlay.url = github:oxalica/rust-overlay;
    flake-utils.url = github:numtide/flake-utils;
  };

  outputs = { self, flake-utils, rust-overlay, nixpkgs }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ (import rust-overlay) ];
        pkgs = import nixpkgs {
          inherit system overlays;
        };
      in
      rec {
        devShell = pkgs.mkShell {
          buildInputs = [
            (pkgs.rust-bin.selectLatestNightlyWith
              (toolchain: toolchain.default.override {
                targets = [ "thumbv6m-none-eabi" ];
                extensions = [ "rust-src" ];
              }))
            pkgs.rust-analyzer
            pkgs.cargo-embed
            pkgs.flip-link
            pkgs.probe-run
            pkgs.rustfmt
          ];
        };
      });
}
