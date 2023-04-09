/* global BROWSER_RUNTIME */

loadScripts = path => {
  let r = require.context(path, false, /\.plutus$/);
  let res = {};
  r.keys().forEach((key) => (res[key] = r(key)));
  return res;
}

let applied_scripts;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  let applied_scripts = loadScripts('Scripts/applied/');
  let unapplied_scripts = loadScripts('Scripts');
} else {
  throw new Error('Not in browser environment, test will fail with no bundler')
}

exports.applied_scripts = applied_scripts;
exports.unapplied_scripts = unapplied_scripts;