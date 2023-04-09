
// scripts = {"alwaysSucceed" : "some script" }
let scripts = {};

function importAll(r) {
  r.keys().forEach((key) => (scripts[key] = r(key)));
}

let scriptsDir = require.context('Scripts', true, /\.plutus$/);
importAll(scriptsDir);

console.log(scripts);

exports.scripts = scripts;


