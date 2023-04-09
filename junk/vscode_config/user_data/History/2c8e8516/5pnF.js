
// scripts = {"alwaysSucceed" : "some script" }
let scripts = {};

let scriptsDir = require.context('Scripts', true, /\.plutus$/);

function importAll(r) {
  r.keys().forEach((key) => (scripts[key] = r(key)));
}

console.log(scripts);

exports.scripts = scripts;

const fs = require("fs");
const path = require("path");
const read_script = fp => {
  return fs.readFileSync(
    path.resolve(__dirname, "../../compiled-scripts/".concat(fp)),
    "utf8"
  );
};

if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  always-fails = require("Scripts/always-fails");
  
} else {


  always-fails = read_script("always-fails.plutus");
}

exports.scripts = scripts;
