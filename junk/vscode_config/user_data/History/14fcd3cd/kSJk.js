/* global BROWSER_RUNTIME */

let scripts;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  let r = require.context('Scripts', 
    true, /* recursively */
     /\.plutus$/);
  let scripts = {};
  r.keys().forEach((key) => (scripts[key] = r(key)));
} else {
  throw new Error('Not in browser environment, test will fail with no bundler')
}

exports.scripts = scripts;