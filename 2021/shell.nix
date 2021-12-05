with import <nixpkgs> {};
let
  java = jdk16;
  gradle = (gradleGen.override {
    inherit java;
  }).gradle_latest;
in
mkShell {
  buildInputs = [java gradle kotlin];
  shellHook = ''
    site() {
      xdg-open "https://adventofcode.com/"
    }
 '';
}
