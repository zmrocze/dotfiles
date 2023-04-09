"use strict";

import { keyHashCredential } from "./Offchain";

import("./Debug.js").then( {keyHashCredential} => {
    console.log("imported");
    console.log(keyHashCredential);
})
