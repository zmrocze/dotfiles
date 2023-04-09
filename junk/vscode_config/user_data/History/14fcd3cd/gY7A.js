/* global BROWSER_RUNTIME */

let scripts;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  scripts = require("Scripts/always-succeeds.plutus");
  
} else {
  const fs = require("fs");
  const path = require("path");
  script = fs.readFileSync(
    path.resolve(__dirname, "../../fixtures/scripts/applied/always-succeeds.plutus"),
    "utf8"
  );
}

exports.scripts = scripts;
