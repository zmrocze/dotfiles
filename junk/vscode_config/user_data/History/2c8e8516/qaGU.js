
// scripts = {"alwaysSucceed" : "some script" }
let scripts = {};

let scriptsDir = require.context('Scripts', true, /\.plutus$/);

function importAll(r) {
  r.keys().forEach((key) => (scripts[key] = r(key)));
}

console.log(scripts);

exports.scripts = scripts;


let scripts = {};
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  scripts["always-fails"] = require("Scripts/always-fails.plutus");
  
} else {
  const fs = require("fs");
  const path = require("path");
  const read_script = fp => {
    return fs.readFileSync(
      path.resolve(__dirname, "../../fixtures/scripts/".concat(fp)),
      "utf8"
    );
  };

  scripts["always-fails"] = read_script("always-fails.plutus");
}

exports.scripts = scripts;
