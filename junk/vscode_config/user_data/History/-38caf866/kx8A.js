
// scripts = {"alwaysSucceed" : "some script" }
let scripts = {};

let scriptsDir = require.context('Scripts', true, /\.plutus$/);

function importAll(r) {
  r.keys().forEach((key) => (cache[key] = r(key)));
}



