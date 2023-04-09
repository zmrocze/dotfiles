
use std::{fmt::Write, num::ParseIntError};

use applyArgs::apply_params_to_script_with_errors;
use uplc::{PlutusData, BigInt, Constr, plutus_data_to_bytes, tx::apply_params_to_script};

pub fn decode_hex(s: &str) -> Result<Vec<u8>, ParseIntError> {
    (0..s.len())
        .step_by(2)
        .map(|i| u8::from_str_radix(&s[i..i + 2], 16))
        .collect()
}

pub fn encode_hex(s: &[u8]) -> String {
    let mut res = String::new();
    let chars : Vec<char> = "0123456789abcdef".chars().collect();
    for ch in s {
        let a = ch / 16;
        let b = ch % 16;
        res.push(chars[usize::from(a)]);
    }
    return res
}

fn main () {
    let plutus_unit : PlutusData = PlutusData::Constr(Constr{tag: 0, any_constructor: None, fields: vec![]});
    let plutus_5 : PlutusData = PlutusData::BigInt(BigInt::Int(pallas_codec::utils::Int::from(5)));
    let plutus_4 : PlutusData = PlutusData::BigInt(BigInt::Int(pallas_codec::utils::Int::from(4)));

    let script0a = "4d01000033222220051200120011";
    let script0a_bytes = decode_hex(script0a).unwrap();    
    let args: Vec<PlutusData> =  vec![plutus_unit];
    let args: PlutusData = PlutusData::Array(args);
    let arg_bytes = plutus_data_to_bytes(&args).unwrap(); 
    print!("[unit] bytes: {:?}\n", arg_bytes);
    print!("{:?}\n", script0a_bytes);
    
    // in js we see         [78, 77, 1, 0, 0, 51, 34, 34, 32, 5, 18, 0, 18, 0, 17]   
    // the first 78 byte shouldnt be there (4d is 77)
    let script0a_ba: [u8; 14] = [77, 1, 0, 0, 51, 34, 34, 32, 5, 18, 0, 18, 0, 17]; // always succeeds
    // let arg_ba: [u8; 6] = [159, 192, 159, 255, 5, 255];
                       // [159, 216, 121, 128, 5, 255]
                       // [unit] = [159, 192, 159, 255, 255]
    let arg_ba: [u8; 4] = [159, 4, 5, 255]; // [4,5]

    let applied_script = apply_params_to_script_with_errors(&arg_ba, &script0a_bytes).unwrap();
    print!("applied\n");
    print!("{:?}\n", applied_script);
    print!("{}\n", applied_script.len());
}