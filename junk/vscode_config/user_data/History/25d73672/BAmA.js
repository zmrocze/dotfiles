"use strict";

// This needs to be asynchronous to load the WASM from CSL
//
// You also need to call `spago bundle-module` to generate the module that is
// imported here. From the repository root, run:
//   spago bundle-module -m <MAIN> --to output.js

console.log("before imports");

let lib;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  lib = require("@emurgo/cardano-serialization-lib-browser");
} else {
  lib = require("@emurgo/cardano-serialization-lib-nodejs");
}
lib = require("@mlabs-haskell/csl-gc-wrapper")(lib);

let txt = require("Scripts/hello.plutus");

console.log("lib?");
console.log(lib);
// console.log("lib.StakeCredential?");
// console.log(lib.StakeCredential);
lib.then(lib1 => {
    console.log("lib.StakeCredential?");
    console.log(lib1.StakeCredential);
    });

console.log("txt?");
console.log(txt);

// const path = require("path");
// console.log("path?");
// console.log(path);


let keyHashCredential = lib.StakeCredential.from_keyhash;
// let keyHashCredential = "hey";
export {keyHashCredential};

// import("../output.js").then(m => m.main());

console.log("app starting");
