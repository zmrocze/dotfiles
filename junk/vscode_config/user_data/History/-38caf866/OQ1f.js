
// scripts = {"alwaysSucceed" : "some script" }
let scripts = {};

function importAll(r) {
  r.keys().forEach((key) => (scripts[key] = r(key)));
}

// let scriptsDir = ;
importAll(require.context('Scripts', true, /\.plutus$/));

console.log(scripts);

exports.scripts = scripts;


