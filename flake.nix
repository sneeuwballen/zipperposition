{
  description = "Zipperposition - A fully automatic theorem prover for typed higher-order and beyond";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs =
    { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
      };

      ocamlPackages = pkgs.ocamlPackages;

      propagatedBuildInputs = with ocamlPackages; [
        zarith
        containers
        containers-data
        msat
        iter
        mtime
        oseq
      ];

      checkInputs = with ocamlPackages; [
        alcotest
        qcheck-core
        qcheck-alcotest
      ];

      zipperposition = ocamlPackages.buildDunePackage {
        pname = "zipperposition";
        version = "2.1";
        src = ./.;
        duneVersion = "3";

        nativeBuildInputs = with ocamlPackages; [
          menhir
        ];

        inherit propagatedBuildInputs;

        preBuild = ''
          find src -name dune -exec sed -i 's/-warn-error //g; s/-a+8 //g; s/-3+8 //g; s/-32 //g' {} +
        '';

        buildPhase = ''
          runHook preBuild
          dune build
          runHook postBuild
        '';

        installPhase = ''
          runHook preInstall
          dune install --prefix $out --libdir $OCAMLFIND_DESTDIR logtk libzipperposition zipperposition zipperposition-tools
          runHook postInstall
        '';

        doCheck = true;
        inherit checkInputs;
        checkPhase = ''
          dune runtest
        '';
      };

      devShell =
        let
          buildInputsAndPathCommon = [
            pkgs.ocaml
            pkgs.dune
            ocamlPackages.ocaml-lsp
            ocamlPackages.utop
            ocamlPackages.cmdliner
            ocamlPackages.re
            ocamlPackages.stdlib-shims
            ocamlPackages.fmt
            ocamlPackages.astring
            ocamlPackages.uutf
            ocamlPackages.either
            ocamlPackages.seq
          ];
        in
        pkgs.mkShell {
          inputsFrom = [ zipperposition ];
          buildInputs = [
            ocamlPackages.alcotest
            ocamlPackages.qcheck-core
            ocamlPackages.qcheck-alcotest
          ]
          ++ buildInputsAndPathCommon;
          shellHook = ''
            export OCAMLPATH=${
              pkgs.lib.makeSearchPath "lib/ocaml/${ocamlPackages.ocaml.version}/site-lib" (
                propagatedBuildInputs ++ checkInputs ++ buildInputsAndPathCommon
              )
            }
            find src -name dune -exec sed -i 's/-warn-error //g; s/-a\+8 //g; s/-3\+8 //g; s/-32 //g' {} +
            echo "Zipperposition development environment"
            echo "Strict warnings have been disabled in dune files."
          '';
        };

    in
    {
      packages.${system}.default = zipperposition;
      devShells.${system}.default = devShell;
    };
}
