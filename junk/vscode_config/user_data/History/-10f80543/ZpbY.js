"use strict";

// This needs to be asynchronous to load the WASM from CSL
//
// You also need to call `spago bundle-module` to generate the module that is
// imported here. From the repository root, run:
//   spago bundle-module -m <MAIN> --to output.js
import("./output.js").then(m => m.main());

let script;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  script = require("Scripts/my-script.plutus");
} else {
  const fs = require("fs");
  const path = require("path");
  script = fs.readFileSync(
    path.resolve(__dirname, "fixtures/scripts/always-succeeds.plutus"),
    "utf8"
  );
}
exports.myScript = script;

console.log("app starting");

