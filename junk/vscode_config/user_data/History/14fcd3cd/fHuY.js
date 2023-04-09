/* global BROWSER_RUNTIME */

loadScripts = path => {
  let r = require.context(path, false, /\.plutus$/);
  let res = {};
  r.keys().forEach((key) => (res[key] = r(key)));
}


let applied_scripts;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  
  
  let r = require.context('Scripts/applied/', false, /\.plutus$/);
  applied_scripts = {};
  r.keys().forEach((key) => (applied_scripts[key] = r(key)));
} else {
  throw new Error('Not in browser environment, test will fail with no bundler')
}

exports.applied_scripts = applied_scripts;
