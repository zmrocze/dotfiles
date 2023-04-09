/* global BROWSER_RUNTIME */

let scripts;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  scripts = require("Scripts/always-succeeds.plutus");

} else {
  throw new Error('Not in browser environment')
}

exports.scripts = scripts;
