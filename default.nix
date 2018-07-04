{ mkDerivation, base, pure, pure-prop, pure-proxy, pure-txt, stdenv }:
mkDerivation {
  pname = "pure-portal";
  version = "0.7.0.0";
  src = ./.;
  libraryHaskellDepends = [ base pure pure-prop pure-proxy pure-txt ];
  homepage = "github.com/grumply/pure-portal";
  description = "";
  license = stdenv.lib.licenses.bsd3;
}