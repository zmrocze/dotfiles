/* global BROWSER_RUNTIME */

let scripts;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  scripts = require("Scripts/always-succeeds.plutus");
  require.context('Scripts/applied/', false, /\.plutus$/);
  const cache = {};

    function importAll(r) {
  r.keys().forEach((key) => (cache[key] = r(key)));
}
} else {
  throw new Error('Not in browser environment, test will fail with no bundler')
}

exports.scripts = scripts;
