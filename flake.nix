{
  description = "Coursework for Performance-Aware Programming";

  inputs = {
    self.submodules = true;
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    crane.url = "github:ipetkov/crane";
  };

  outputs = { self, nixpkgs, flake-utils, rust-overlay, crane }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ (import rust-overlay) ];
        };

        rustToolchain = pkgs.rust-bin.nightly.latest.default;

        craneLib = (crane.mkLib pkgs).overrideToolchain rustToolchain;

        # Common build inputs for all derivations
        commonArgs = {
          version = "0.1.0"; # Set explicitly to avoid warnings
          src = pkgs.lib.cleanSourceWith {
            src = ./.;
            name = "source";
            filter = path: type: (builtins.match ".*json$" path != null || pkgs.lib.hasInfix "/computer_enhance" path || craneLib.filterCargoSources path type);
          };
          strictDeps = true;

          buildInputs = [
            pkgs.openssl
          ];

          nativeBuildInputs = [
            pkgs.pkg-config
          ];
        };

        # Build just the cargo dependencies
        cargoArtifacts = craneLib.buildDepsOnly commonArgs;

        # Build the actual package
        sim-wrapper = craneLib.buildPackage (commonArgs // {
          inherit cargoArtifacts;
        });
      in
      {
        packages = {
          default = sim-wrapper;
          sim-wrapper = sim-wrapper;
        };

        apps = {
          default = {
            type = "app";
            program = "${sim-wrapper}/bin/sim-wrapper";
            meta = sim-wrapper.meta;
          };
          sim-wrapper = {
            type = "app";
            program = "${sim-wrapper}/bin/sim-wrapper";
            meta = sim-wrapper.meta;
          };
        };

        devShells = {
          default = craneLib.devShell {
            # Inherit the build inputs from commonArgs
            buildInputs = commonArgs.buildInputs;
            nativeBuildInputs = commonArgs.nativeBuildInputs;

            # Additional dev tools
            packages = [
              rustToolchain
              pkgs.rust-analyzer
            ];

            RUST_SRC_PATH = "${rustToolchain}/lib/rustlib/src/rust/library";
          };

          ci = pkgs.mkShell {
            buildInputs = commonArgs.buildInputs ++ [
              pkgs.nodePackages.markdown-link-check
              pkgs.nixpkgs-fmt
              rustToolchain
            ];

            nativeBuildInputs = commonArgs.nativeBuildInputs;

            RUST_SRC_PATH = "${rustToolchain}/lib/rustlib/src/rust/library";
          };
        };

        # Optional: add checks for CI
        checks = {
          inherit sim-wrapper;

          # Run cargo fmt check
          fmt = craneLib.cargoFmt {
            inherit (commonArgs) src;
          };

          # Run clippy
          clippy = craneLib.cargoClippy (commonArgs // {
            inherit cargoArtifacts;
            cargoClippyExtraArgs = "--all-targets -- --deny warnings";
          });

          # Run tests
          tests = craneLib.cargoTest (commonArgs // {
            inherit cargoArtifacts;
          });
        };
      });
}
