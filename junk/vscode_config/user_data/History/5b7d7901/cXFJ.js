
let lib;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  lib = require("@emurgo/cardano-serialization-lib-browser");
} else {
  lib = require("@emurgo/cardano-serialization-lib-nodejs");
}
lib = require("@mlabs-haskell/csl-gc-wrapper")(lib);

console.log("lib?");
console.log(lib);
console.log("lib.StakeCredential?");
console.log(lib.StakeCredential);

let keyHashCredential = lib.StakeCredential.from_keyhash;
// let keyHashCredential = "hey";
export {keyHashCredential};