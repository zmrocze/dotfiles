"use strict";

import("./Debug.js").then( m => {
    let {keyHashCredential} = m;
    console.log("imported");
    console.log(keyHashCredential);
    console.log("done");
})
