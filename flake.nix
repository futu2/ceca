{
  description = "A Haskell project";

  inputs.hix.url = "github:tek/hix?ref=0.9.1";

  outputs = {hix, ...}: hix.lib.flake {
    hackage.versionFile = "ops/version.nix";
    compiler = "ghc910";

    cabal = {
      license = "BSD-2-Clause-Patent";
      license-file = "LICENSE";
      author = "futu";
      ghc-options = ["-Wall"];
    };

    packages.ceca = {
      src = ./.;
      cabal.meta.synopsis = "A Language";

      library = {
        enable = true;
        dependencies = [
          "containers"
          "megaparsec"
          "mtl"
          "text"
        ];
      };

      executable.enable = true;

      test = {
        enable = true;
        dependencies = [
          "hedgehog >= 1.1 && < 1.5"
          "tasty ^>= 1.4"
          "tasty-hedgehog >= 1.3 && < 1.5"
        ];
      };

    };
  };
}
