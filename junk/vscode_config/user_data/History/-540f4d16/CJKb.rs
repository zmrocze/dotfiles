mod utils;

use wasm_bindgen::prelude::*;
use uplc::{tx::error::Error, PlutusData};
 // use uplc::apply_params_to_script as uplc_apply_params_to_script;
use uplc::{
    ast::{DeBruijn, Program},
    // machine::cost_model::ExBudget,
    // PlutusData,
};
use pallas_primitives::{Fragment};

// When the `wee_alloc` feature is enabled, use `wee_alloc` as the global
// allocator.
// #[cfg(feature = "wee_alloc")]
// #[global_allocator]
// static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

/// Apply parameters, provided as PlutusData list to a script. Returns string error.
/// Err(e) translates to an exception on javascript side.
#[wasm_bindgen]
pub fn apply_params_to_script(
    params_bytes: &[u8], // PlutusData array
    plutus_script_bytes: &[u8],
) -> Result<Vec<u8>, String> {
    uplc::tx::apply_params_to_script(params_bytes, plutus_script_bytes).map_err(|e| e.to_string())
}

#[wasm_bindgen]
pub fn apply_params_to_script_with_errors(
    params_bytes: &[u8], // PlutusData array
    plutus_script_bytes: &[u8],
) -> Result<Vec<u8>, String> {
    let params = match PlutusData::decode_fragment(params_bytes) {
        Ok(PlutusData::Array(res))  => Ok(res),
        Ok(_) => Err("Couldn't decode as a plutus Array".to_string()),
        Err(e) => Err(e.to_string())
    }?;

    let mut buffer = Vec::new();
    let mut program = Program::<DeBruijn>::from_cbor(plutus_script_bytes, &mut buffer)
        .map_err(|e| e.to_string())?;
    
    println!("{:?}", params);

    for param in params {
        program = program.apply_data(param);
    }
    

    match program.to_cbor() {
        Ok(res) => Ok(res),
        Err(_) => Err("Couldn't encode resulting script as CBOR.".to_string()),
    }
}
