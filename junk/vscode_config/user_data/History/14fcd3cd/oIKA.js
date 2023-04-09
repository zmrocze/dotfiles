/* global BROWSER_RUNTIME */

loadScripts = path => {
  let r = require.context(path, 
      true, /* recursively */
       /\.plutus$/);
  let res = {};
  r.keys().forEach((key) => (res[key] = r(key)));
  return res;
}

let scripts;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  let scripts = loadScripts('Scripts');
} else {
  throw new Error('Not in browser environment, test will fail with no bundler')
}

exports.scripts = scripts;