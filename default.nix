{ mkDerivation, base, pure, pure-cond, pure-prop, pure-proxy, pure-txt, pure-transition, stdenv }:
mkDerivation {
  pname = "pure-portal";
  version = "0.7.0.0";
  src = ./.;
  libraryHaskellDepends = [ base pure pure-cond pure-prop pure-proxy pure-txt pure-transition ];
  homepage = "github.com/grumply/pure-portal";
  description = "";
  license = stdenv.lib.licenses.bsd3;
}