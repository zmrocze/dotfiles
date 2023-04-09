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

  scripts["always-fails-big-arg.plutus"] = require("Scripts/applied/always-fails-big-arg.plutus")
  scripts["always-fails-no-args.plutus"] = require("Scripts/applied/always-fails-no-args.plutus")
  scripts["always-fails-unit.plutus"] = require("Scripts/applied/always-fails-unit.plutus")
  scripts["always-succeeds-v2-big-arg.plutus"] = require("Scripts/applied/always-succeeds-v2-big-arg.plutus")
  scripts["always-succeeds-v2-no-args.plutus"] = require("Scripts/applied/always-succeeds-v2-no-args.plutus")
  scripts["always-succeeds-v2-unit.plutus"] = require("Scripts/applied/always-succeeds-v2-unit.plutus")
  scripts["check-datum-is-inline-big-arg.plutus"] = require("Scripts/applied/check-datum-is-inline-big-arg.plutus")
  scripts["check-datum-is-inline-no-args.plutus"] = require("Scripts/applied/check-datum-is-inline-no-args.plutus")
  scripts["check-datum-is-inline-unit.plutus"] = require("Scripts/applied/check-datum-is-inline-unit.plutus")
  scripts["include-datum-big-arg.plutus"] = require("Scripts/applied/include-datum-big-arg.plutus")
  scripts["include-datum-no-args.plutus"] = require("Scripts/applied/include-datum-no-args.plutus")
  scripts["include-datum-unit.plutus"] = require("Scripts/applied/include-datum-unit.plutus")
  scripts["one-shot-minting-big-arg.plutus"] = require("Scripts/applied/one-shot-minting-big-arg.plutus")
  scripts["one-shot-minting-no-args.plutus"] = require("Scripts/applied/one-shot-minting-no-args.plutus")
  scripts["one-shot-minting-unit.plutus"] = require("Scripts/applied/one-shot-minting-unit.plutus")
  scripts["one-shot-minting-v2-big-arg.plutus"] = require("Scripts/applied/one-shot-minting-v2-big-arg.plutus")
  scripts["one-shot-minting-v2-no-args.plutus"] = require("Scripts/applied/one-shot-minting-v2-no-args.plutus")
  scripts["one-shot-minting-v2-unit.plutus"] = require("Scripts/applied/one-shot-minting-v2-unit.plutus")
  scripts["redeemer1-validator-big-arg.plutus"] = require("Scripts/applied/redeemer1-validator-big-arg.plutus")
  scripts["redeemer1-validator-no-args.plutus"] = require("Scripts/applied/redeemer1-validator-no-args.plutus")
  scripts["redeemer1-validator-unit.plutus"] = require("Scripts/applied/redeemer1-validator-unit.plutus")

} else {
  const fs = require("fs");
  const path = require("path");
  const read_script = fp => {
    return path.resolve(__dirname, "../../fixtures/scripts/".concat(fp))
  }
  script = fs.readFileSync(
    
    "utf8"
  );
  scripts["always-fails"] = require("Scripts/always-fails.plutus")
  scripts["include-datum"] = require("Scripts/include-datum.plutus")
  scripts["one-shot-minting"] = require("Scripts/one-shot-minting.plutus")
  scripts["redeemer1-validator"] = require("Scripts/redeemer1-validator.plutus")
  scripts["always-succeeds-v2"] = require("Scripts/always-succeeds-v2.plutus")
  scripts["one-shot-minting-v2"] = require("Scripts/one-shot-minting-v2.plutus")
  scripts["check-datum-is-inline"] = require("Scripts/check-datum-is-inline.plutus")

  scripts["always-fails-big-arg.plutus"] = require("Scripts/applied/always-fails-big-arg.plutus")
  scripts["always-fails-no-args.plutus"] = require("Scripts/applied/always-fails-no-args.plutus")
  scripts["always-fails-unit.plutus"] = require("Scripts/applied/always-fails-unit.plutus")
  scripts["always-succeeds-v2-big-arg.plutus"] = require("Scripts/applied/always-succeeds-v2-big-arg.plutus")
  scripts["always-succeeds-v2-no-args.plutus"] = require("Scripts/applied/always-succeeds-v2-no-args.plutus")
  scripts["always-succeeds-v2-unit.plutus"] = require("Scripts/applied/always-succeeds-v2-unit.plutus")
  scripts["check-datum-is-inline-big-arg.plutus"] = require("Scripts/applied/check-datum-is-inline-big-arg.plutus")
  scripts["check-datum-is-inline-no-args.plutus"] = require("Scripts/applied/check-datum-is-inline-no-args.plutus")
  scripts["check-datum-is-inline-unit.plutus"] = require("Scripts/applied/check-datum-is-inline-unit.plutus")
  scripts["include-datum-big-arg.plutus"] = require("Scripts/applied/include-datum-big-arg.plutus")
  scripts["include-datum-no-args.plutus"] = require("Scripts/applied/include-datum-no-args.plutus")
  scripts["include-datum-unit.plutus"] = require("Scripts/applied/include-datum-unit.plutus")
  scripts["one-shot-minting-big-arg.plutus"] = require("Scripts/applied/one-shot-minting-big-arg.plutus")
  scripts["one-shot-minting-no-args.plutus"] = require("Scripts/applied/one-shot-minting-no-args.plutus")
  scripts["one-shot-minting-unit.plutus"] = require("Scripts/applied/one-shot-minting-unit.plutus")
  scripts["one-shot-minting-v2-big-arg.plutus"] = require("Scripts/applied/one-shot-minting-v2-big-arg.plutus")
  scripts["one-shot-minting-v2-no-args.plutus"] = require("Scripts/applied/one-shot-minting-v2-no-args.plutus")
  scripts["one-shot-minting-v2-unit.plutus"] = require("Scripts/applied/one-shot-minting-v2-unit.plutus")
  scripts["redeemer1-validator-big-arg.plutus"] = require("Scripts/applied/redeemer1-validator-big-arg.plutus")
  scripts["redeemer1-validator-no-args.plutus"] = require("Scripts/applied/redeemer1-validator-no-args.plutus")
  scripts["redeemer1-validator-unit.plutus"] = require("Scripts/applied/redeemer1-validator-unit.plutus")

}

exports.scripts = scripts;