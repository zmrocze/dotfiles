/* global BROWSER_RUNTIME */

let lib;
if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
  lib = require("@emurgo/cardano-serialization-lib-browser");
} else {
  lib = require("@emurgo/cardano-serialization-lib-nodejs");
}

console.log(typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME);

const uplc = require("uplc");

/**
* @param {Uint8Array} params_bytes  -- parameter list encoded as PlutusData List
* @param {Uint8Array} plutus_script_bytes
* @returns {Uint8Array}
* uplc.apply_params_to_script;
*/

/**
* @param {PlutusData} args
* @param {PlutusScript} script
* @returns {PlutusScript}
*/
exports.apply_params_to_script = args => script => {
    try {
    let scriptBytes = script.to_bytes()
    let argsBytes = args.to_bytes()
    let appliedScript
    try { 
        appliedScript = uplc.apply_params_to_script(argsBytes, scriptBytes)
    } catch (e) {
        console.log(e)
        throw e
    }
    return lib.PlutusScript.from_bytes(appliedScript)
    } catch (e1) {
        console.log(e1)
        throw e1
    }
}

/**
* @param {PlutusData} args
* @param {PlutusScript} script
* @returns {Either String PlutusScript}
*/
exports.apply_params_to_script_with_errors = left => right => args => script => {
  let version = script.language_version();
  try {
    let scriptBytes = script.bytes(); // raw bytes, not cbor
    let argsBytes = args.to_bytes(); // cbor

    let appliedScript
    try { 
        appliedScript = uplc.apply_params_to_script_with_errors(argsBytes, scriptBytes)
    } catch (e) {
        return left(e.toString())
    }

    

  } catch (e1) {
    console.log(e1)
    throw e1
  }
  return right(lib.PlutusScript.new_with_version(appliedScript, version))
}