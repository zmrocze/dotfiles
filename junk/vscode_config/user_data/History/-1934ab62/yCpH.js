const fs = require("fs");
const path = require("path");
script = fs.readFileSync(
    path.resolve(__dirname, "../../fixtures/scripts/always-succeeds.plutus"),
    "utf8"
  );
// exports.myscript = require("../../fixtures/scripts/always-succeeds-2.plutus");
exports.myscript = script
