/* global BROWSER_RUNTIME */

let lib;
let apply_args;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  lib = require("@emurgo/cardano-serialization-lib-browser");
  apply_args = require("apply-args-browser");
} else {
  lib = require("@emurgo/cardano-serialization-lib-nodejs");
  apply_args = require("apply-args-nodejs");
}

/**
 * @param {} left
 * @param {} right
 * @param {PlutusData} args
 * @param {PlutusScript} script
 * @returns {Either String PlutusScript}
 */
exports.apply_params_to_script = left => right => args => script => {
  let toHex = ba => {
    return [...new Uint8Array (ba)].map(x => x.toString(16).padStart(2, '0'))
        .join('');
  }
    
  let version = script.language_version();
  let appliedScript;
  try {
    let scriptBytes = script.bytes(); // raw bytes
    let argsBytes = args.to_bytes(); // cbor
    console.log(toHex(argsBytes));
    try {
      appliedScript = apply_args.apply_params_to_script_no_panic(
        argsBytes,
        scriptBytes
      );
    } catch (e) {
      return left("Error applying argument to script: ".concat(e.toString()));
    }
  } catch (e1) {
    return left("Error serializing arguments: ".concat(e1.toString()));
  }
  return right(lib.PlutusScript.new_with_version(appliedScript, version));
};
