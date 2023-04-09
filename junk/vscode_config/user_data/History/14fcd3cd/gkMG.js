/* global BROWSER_RUNTIME */

let scripts;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  
  scripts["always-fails"] = require("Scripts/always-fails.plutus")
  scripts["include-datum"] = require("Scripts/include-datum.plutus")
  scripts["one-shot-minting"] = require("Scripts/one-shot-minting.plutus")
  scripts["redeemer1-validator"] = require("Scripts/redeemer1-validator.plutus")
  scripts["always-succeeds-v2"] = require("Scripts/always-succeeds-v2.plutus")
  scripts["one-shot-minting-v2"] = require("Scripts/one-shot-minting-v2.plutus")
  scripts["check-datum-is-inline"] = require("Scripts/check-datum-is-inline.plutus")

    

} else {
  throw new Error('Not in browser environment')
}

exports.scripts = scripts;