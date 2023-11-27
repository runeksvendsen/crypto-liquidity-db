{ system ? builtins.currentSystem
}:
let
  pkgs =
    import (builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/577152136046cf81ebd6ef6fcf932cf333762db6.tar.gz";
      sha256 = "1mrynf40p64hnqqpy1lpdmg6ilksci2whmfa3nbg3lp9nf0zxcbq";
    }) { system = system; };
  pkgsOld =
    import (builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/release-22.11.tar.gz";
      sha256 = "1xi53rlslcprybsvrmipm69ypd3g3hr7wkxvzc73ag8296yclyll";
    }) {};
in
  pkgs.mkShell {
    nativeBuildInputs = [
      pkgs.haskell.compiler.ghc810
      pkgs.stack
      pkgsOld.cabal-install # there's a regression in 3.10 that prevents building haskell-language-server so we use an earlier version: https://github.com/haskell/cabal/issues/9190
      pkgs.git
      pkgs.postgresql
      pkgs.zlib
    ];
}
