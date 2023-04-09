
script = fs.readFileSync(
    path.resolve(__dirname, "../../fixtures/scripts/my-script.plutus"),
    "utf8"
  );
// exports.myscript = require("../../fixtures/scripts/always-succeeds-2.plutus");
exports.myscript = script
