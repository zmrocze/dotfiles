// Generated by purs bundle 0.14.9
var PS = {};
(function(exports) {
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
  lib = require("@mlabs-haskell/csl-gc-wrapper")(lib);
  apply_args = require("@mlabs-haskell/csl-gc-wrapper")(apply_args);

  /**
 * @param {} left
 * @param {} right
 * @param {PlutusData} args
 * @param {PlutusScript} script
 * @returns {Either String PlutusScript}
 */  
  exports.apply_params_to_script = left => right => args => script => {
    let version = script.language_version();
    let appliedScript;
    try {
      let scriptBytes = script.bytes(); // raw bytes
      let argsBytes = args.to_bytes(); // cbor

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
})(PS["Ctl.Internal.ApplyArgs"] = PS["Ctl.Internal.ApplyArgs"] || {});
(function($PS) {
  // Generated by purs version 0.14.9
  "use strict";
  $PS["Control.Bind"] = $PS["Control.Bind"] || {};
  var exports = $PS["Control.Bind"];
  var bind = function (dict) {
      return dict.bind;
  };
  exports["bind"] = bind;
})(PS);
(function(exports) {
  const call = property => object => object[property]();

  const callMaybe = property => maybe => object => {
    const res = object[property]();
    return res != null ? maybe.just(res) : maybe.nothing;
  };

  // Classes like TransactionInputs are just monomorphic containers with `len`
  // and `get()` methods. This function abstracts away converting them to Array
  // of something.
  const containerExtractor = obj => {
    const res = [];

    for (let i = 0; i < obj.len(); i++) {
      res.push(obj.get(i));
    }

    return res;
  };                                                
  exports.plutusScriptBytes = call("bytes");
  exports.plutusScriptVersion = call("language_version");
})(PS["Ctl.Internal.Deserialization.WitnessSet"] = PS["Ctl.Internal.Deserialization.WitnessSet"] || {});
(function(exports) {
  /* global BROWSER_RUNTIME */

  let lib;
  if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
    lib = require("@emurgo/cardano-serialization-lib-browser");
  } else {
    lib = require("@emurgo/cardano-serialization-lib-nodejs");
  }
  lib = require("@mlabs-haskell/csl-gc-wrapper")(lib);

  exports._convertLanguage = langCtors => cslLang => {
    if (cslLang.kind() == lib.LanguageKind.PlutusV1) {
      return langCtors.plutusV1;
    } else if (cslLang.kind() == lib.LanguageKind.PlutusV2) {
      return langCtors.plutusV2;
    } else {
      throw "_convertLanguage: Unsupported language kind: " + cslLang.kind();
    }
  };
})(PS["Ctl.Internal.Deserialization.Language"] = PS["Ctl.Internal.Deserialization.Language"] || {});
(function($PS) {
  // Generated by purs version 0.14.9
  "use strict";
  $PS["Ctl.Internal.Types.Scripts"] = $PS["Ctl.Internal.Types.Scripts"] || {};
  var exports = $PS["Ctl.Internal.Types.Scripts"];
  var PlutusV1 = (function () {
      function PlutusV1() {

      };
      PlutusV1.value = new PlutusV1();
      return PlutusV1;
  })();
  var PlutusV2 = (function () {
      function PlutusV2() {

      };
      PlutusV2.value = new PlutusV2();
      return PlutusV2;
  })();
  var PlutusScript = function (x) {
      return x;
  };
  exports["PlutusScript"] = PlutusScript;
  exports["PlutusV1"] = PlutusV1;
  exports["PlutusV2"] = PlutusV2;
})(PS);
(function($PS) {
  // Generated by purs version 0.14.9
  "use strict";
  $PS["Ctl.Internal.Deserialization.Language"] = $PS["Ctl.Internal.Deserialization.Language"] || {};
  var exports = $PS["Ctl.Internal.Deserialization.Language"];
  var $foreign = $PS["Ctl.Internal.Deserialization.Language"];
  var Ctl_Internal_Types_Scripts = $PS["Ctl.Internal.Types.Scripts"];                
  var convertLanguage = $foreign["_convertLanguage"]({
      plutusV1: Ctl_Internal_Types_Scripts.PlutusV1.value,
      plutusV2: Ctl_Internal_Types_Scripts.PlutusV2.value
  });
  exports["convertLanguage"] = convertLanguage;
})(PS);
(function($PS) {
  // Generated by purs version 0.14.9
  "use strict";
  $PS["Data.Tuple"] = $PS["Data.Tuple"] || {};
  var exports = $PS["Data.Tuple"];                 
  var Tuple = (function () {
      function Tuple(value0, value1) {
          this.value0 = value0;
          this.value1 = value1;
      };
      Tuple.create = function (value0) {
          return function (value1) {
              return new Tuple(value0, value1);
          };
      };
      return Tuple;
  })();
  var snd = function (v) {
      return v.value1;
  };
  var fst = function (v) {
      return v.value0;
  };
  var curry = function (f) {
      return function (a) {
          return function (b) {
              return f(new Tuple(a, b));
          };
      };
  };
  exports["Tuple"] = Tuple;
  exports["fst"] = fst;
  exports["snd"] = snd;
  exports["curry"] = curry;
})(PS);
(function($PS) {
  // Generated by purs version 0.14.9
  "use strict";
  $PS["Ctl.Internal.Deserialization.WitnessSet"] = $PS["Ctl.Internal.Deserialization.WitnessSet"] || {};
  var exports = $PS["Ctl.Internal.Deserialization.WitnessSet"];
  var $foreign = $PS["Ctl.Internal.Deserialization.WitnessSet"];
  var Ctl_Internal_Deserialization_Language = $PS["Ctl.Internal.Deserialization.Language"];
  var Ctl_Internal_Types_Scripts = $PS["Ctl.Internal.Types.Scripts"];
  var Data_Tuple = $PS["Data.Tuple"];
  var convertPlutusScript = function (plutusScript) {
      var language = Ctl_Internal_Deserialization_Language.convertLanguage($foreign.plutusScriptVersion(plutusScript));
      return Data_Tuple.curry(Ctl_Internal_Types_Scripts.PlutusScript)($foreign.plutusScriptBytes(plutusScript))(language);
  };
  exports["convertPlutusScript"] = convertPlutusScript;
})(PS);
(function(exports) {
  /* global BROWSER_RUNTIME */

  let lib;
  if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
    lib = require("@emurgo/cardano-serialization-lib-browser");
  } else {
    lib = require("@emurgo/cardano-serialization-lib-nodejs");
  }
  lib = require("@mlabs-haskell/csl-gc-wrapper")(lib);

  exports._mkPlutusData_bytes = bytes => lib.PlutusData.new_bytes(bytes);
  exports._mkPlutusData_list = list => lib.PlutusData.new_list(list);
  exports._mkPlutusData_map = list => lib.PlutusData.new_map(list);
  exports._mkPlutusData_integer = int => lib.PlutusData.new_integer(int);
  exports._mkPlutusData_constr = constr =>
    lib.PlutusData.new_constr_plutus_data(constr);

  exports._packPlutusList = containerHelper => elems =>
    containerHelper.pack(lib.PlutusList, elems);
  exports._mkConstrPlutusData = n => list => lib.ConstrPlutusData.new(n, list);

  exports._bigIntFromString = maybe => str => {
    try {
      return maybe.just(lib.BigInt.from_str(str));
    } catch (_) {
      return maybe.nothing;
    }
  };

  exports._packMap = first => second => kvs => {
    const res = lib.PlutusMap.new();
    for (let kv of kvs) {
      res.insert(first(kv), second(kv));
    }
    return res;
  };
})(PS["Ctl.Internal.Serialization.PlutusData"] = PS["Ctl.Internal.Serialization.PlutusData"] || {});
(function(exports) {
  // Abstracts away unpacking elements from a monomorphic container.
  const unpack = obj => {
    const res = [];

    for (let i = 0; i < obj.len(); i++) {
      res.push(obj.get(i));
    }

    return res;
  };

  const unpackFromProperty = prop => obj => unpack(obj[prop]());

  exports._containerHelper = r => ({
    unpack,
    unpackFromProperty,
    // unpacks an associative container where keys are stored in .keys()
    // and values for that keys might be missing.
    unpackKeyIndexed: obj => {
      const res = [];
      for (let i = 0; i < obj.len(); i++) {
        let k = obj.keys().get(i);
        let v = obj.get(k);
        if (v == null) continue;
        res.push(r.tuple(k)(v));
      }
      return res;
    },
    // Abstracts away packing array of something into a monomorphic container.
    pack: (container, elements) => {
      const res = container.new();
      elements.forEach(elem => res.add(elem));
      return res;
    },
    // Abstracts away packing a list of KV-pairs into a map-like structure.
    packMap: (container, entries) => {
      const res = container.new();
      entries.forEach(entry => {
        const [key, value] = r.untuple(entry);
        res.insert(key, value);
      });
      return res;
    },
  });
})(PS["Ctl.Internal.FfiHelpers"] = PS["Ctl.Internal.FfiHelpers"] || {});
(function($PS) {
  // Generated by purs version 0.14.9
  "use strict";
  $PS["Control.Semigroupoid"] = $PS["Control.Semigroupoid"] || {};
  var exports = $PS["Control.Semigroupoid"];
  var semigroupoidFn = {
      compose: function (f) {
          return function (g) {
              return function (x) {
                  return f(g(x));
              };
          };
      }
  };
  exports["semigroupoidFn"] = semigroupoidFn;
})(PS);
(function($PS) {
  // Generated by purs version 0.14.9
  "use strict";
  $PS["Control.Category"] = $PS["Control.Category"] || {};
  var exports = $PS["Control.Category"];
  var Control_Semigroupoid = $PS["Control.Semigroupoid"];                
  var identity = function (dict) {
      return dict.identity;
  };
  var categoryFn = {
      identity: function (x) {
          return x;
      },
      Semigroupoid0: function () {
          return Control_Semigroupoid.semigroupoidFn;
      }
  };
  exports["identity"] = identity;
  exports["categoryFn"] = categoryFn;
})(PS);
(function($PS) {
  // Generated by purs version 0.14.9
  "use strict";
  $PS["Data.Maybe"] = $PS["Data.Maybe"] || {};
  var exports = $PS["Data.Maybe"];
  var Control_Category = $PS["Control.Category"];  
  var Nothing = (function () {
      function Nothing() {

      };
      Nothing.value = new Nothing();
      return Nothing;
  })();
  var Just = (function () {
      function Just(value0) {
          this.value0 = value0;
      };
      Just.create = function (value0) {
          return new Just(value0);
      };
      return Just;
  })();
  var maybe = function (v) {
      return function (v1) {
          return function (v2) {
              if (v2 instanceof Nothing) {
                  return v;
              };
              if (v2 instanceof Just) {
                  return v1(v2.value0);
              };
              throw new Error("Failed pattern match at Data.Maybe (line 230, column 1 - line 230, column 51): " + [ v.constructor.name, v1.constructor.name, v2.constructor.name ]);
          };
      };
  };
  var fromMaybe = function (a) {
      return maybe(a)(Control_Category.identity(Control_Category.categoryFn));
  };
  var fromJust = function () {
      return function (v) {
          if (v instanceof Just) {
              return v.value0;
          };
          throw new Error("Failed pattern match at Data.Maybe (line 281, column 1 - line 281, column 46): " + [ v.constructor.name ]);
      };
  };
  exports["Nothing"] = Nothing;
  exports["Just"] = Just;
  exports["fromMaybe"] = fromMaybe;
  exports["fromJust"] = fromJust;
})(PS);
(function($PS) {
  // Generated by purs version 0.14.9
  "use strict";
  $PS["Ctl.Internal.FfiHelpers"] = $PS["Ctl.Internal.FfiHelpers"] || {};
  var exports = $PS["Ctl.Internal.FfiHelpers"];
  var $foreign = $PS["Ctl.Internal.FfiHelpers"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Tuple = $PS["Data.Tuple"];                
  var untuple = function (v) {
      return [ v.value0, v.value1 ];
  };
  var maybeFfiHelper = {
      nothing: Data_Maybe.Nothing.value,
      just: Data_Maybe.Just.create,
      from: Data_Maybe.fromMaybe
  };
  var containerHelper = $foreign["_containerHelper"]({
      untuple: untuple,
      tuple: Data_Tuple.Tuple.create
  });
  exports["maybeFfiHelper"] = maybeFfiHelper;
  exports["containerHelper"] = containerHelper;
})(PS);
(function($PS) {
  // Generated by purs version 0.14.9
  "use strict";
  $PS["Ctl.Internal.Types.PlutusData"] = $PS["Ctl.Internal.Types.PlutusData"] || {};
  var exports = $PS["Ctl.Internal.Types.PlutusData"];
  var Constr = (function () {
      function Constr(value0, value1) {
          this.value0 = value0;
          this.value1 = value1;
      };
      Constr.create = function (value0) {
          return function (value1) {
              return new Constr(value0, value1);
          };
      };
      return Constr;
  })();
  var $$Map = (function () {
      function $$Map(value0) {
          this.value0 = value0;
      };
      $$Map.create = function (value0) {
          return new $$Map(value0);
      };
      return $$Map;
  })();
  var List = (function () {
      function List(value0) {
          this.value0 = value0;
      };
      List.create = function (value0) {
          return new List(value0);
      };
      return List;
  })();
  var Integer = (function () {
      function Integer(value0) {
          this.value0 = value0;
      };
      Integer.create = function (value0) {
          return new Integer(value0);
      };
      return Integer;
  })();
  var Bytes = (function () {
      function Bytes(value0) {
          this.value0 = value0;
      };
      Bytes.create = function (value0) {
          return new Bytes(value0);
      };
      return Bytes;
  })();
  exports["Constr"] = Constr;
  exports["Map"] = $$Map;
  exports["List"] = List;
  exports["Integer"] = Integer;
  exports["Bytes"] = Bytes;
})(PS);
(function(exports) {
  
  var bigInt =require("big-integer");

  exports.toBase = function(base) {
    return function (x) {
      return x.toString(base);
    };
  };
})(PS["Data.BigInt"] = PS["Data.BigInt"] || {});
(function($PS) {
  // Generated by purs version 0.14.9
  "use strict";
  $PS["Data.BigInt"] = $PS["Data.BigInt"] || {};
  var exports = $PS["Data.BigInt"];
  var $foreign = $PS["Data.BigInt"];                                                       
  var toString = $foreign.toBase(10);
  exports["toString"] = toString;
})(PS);
(function(exports) {
  "use strict";

  exports.arrayMap = function (f) {
    return function (arr) {
      var l = arr.length;
      var result = new Array(l);
      for (var i = 0; i < l; i++) {
        result[i] = f(arr[i]);
      }
      return result;
    };
  };
})(PS["Data.Functor"] = PS["Data.Functor"] || {});
(function($PS) {
  // Generated by purs version 0.14.9
  "use strict";
  $PS["Data.Functor"] = $PS["Data.Functor"] || {};
  var exports = $PS["Data.Functor"];
  var $foreign = $PS["Data.Functor"];                
  var map = function (dict) {
      return dict.map;
  };
  var mapFlipped = function (dictFunctor) {
      return function (fa) {
          return function (f) {
              return map(dictFunctor)(f)(fa);
          };
      };
  };
  var functorArray = {
      map: $foreign.arrayMap
  };
  exports["map"] = map;
  exports["mapFlipped"] = mapFlipped;
  exports["functorArray"] = functorArray;
})(PS);
(function($PS) {
  // Generated by purs version 0.14.9
  "use strict";
  $PS["Ctl.Internal.Serialization.PlutusData"] = $PS["Ctl.Internal.Serialization.PlutusData"] || {};
  var exports = $PS["Ctl.Internal.Serialization.PlutusData"];
  var $foreign = $PS["Ctl.Internal.Serialization.PlutusData"];
  var Ctl_Internal_FfiHelpers = $PS["Ctl.Internal.FfiHelpers"];
  var Ctl_Internal_Types_PlutusData = $PS["Ctl.Internal.Types.PlutusData"];
  var Data_BigInt = $PS["Data.BigInt"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Tuple = $PS["Data.Tuple"];                
  var convertBigInt = function (n) {
      return Data_Maybe.fromJust()($foreign["_bigIntFromString"](Ctl_Internal_FfiHelpers.maybeFfiHelper)(Data_BigInt.toString(n)));
  };
  var convertPlutusInteger = function (n) {
      return $foreign["_mkPlutusData_integer"](convertBigInt(n));
  };
  var convertPlutusMap = function (mp) {
      var entries = Data_Functor.mapFlipped(Data_Functor.functorArray)(mp)(function (v) {
          return new Data_Tuple.Tuple(convertPlutusData(v.value0), convertPlutusData(v.value1));
      });
      return $foreign["_mkPlutusData_map"]($foreign["_packMap"](Data_Tuple.fst)(Data_Tuple.snd)(entries));
  };
  var convertPlutusList = function (x) {
      return $foreign["_mkPlutusData_list"]($foreign["_packPlutusList"](Ctl_Internal_FfiHelpers.containerHelper)(Data_Functor.map(Data_Functor.functorArray)(convertPlutusData)(x)));
  };
  var convertPlutusData = function (x) {
      if (x instanceof Ctl_Internal_Types_PlutusData.Constr) {
          return convertConstr(x.value0)(x.value1);
      };
      if (x instanceof Ctl_Internal_Types_PlutusData["Map"]) {
          return convertPlutusMap(x.value0);
      };
      if (x instanceof Ctl_Internal_Types_PlutusData.List) {
          return convertPlutusList(x.value0);
      };
      if (x instanceof Ctl_Internal_Types_PlutusData.Integer) {
          return convertPlutusInteger(x.value0);
      };
      if (x instanceof Ctl_Internal_Types_PlutusData.Bytes) {
          return $foreign["_mkPlutusData_bytes"](x.value0);
      };
      throw new Error("Failed pattern match at Ctl.Internal.Serialization.PlutusData (line 31, column 23 - line 36, column 37): " + [ x.constructor.name ]);
  };
  var convertConstr = function (alt) {
      return function (list) {
          return $foreign["_mkPlutusData_constr"]($foreign["_mkConstrPlutusData"](alt)($foreign["_packPlutusList"](Ctl_Internal_FfiHelpers.containerHelper)(Data_Functor.map(Data_Functor.functorArray)(convertPlutusData)(list))));
      };
  };
  exports["convertPlutusData"] = convertPlutusData;
})(PS);
(function(exports) {
  /* global BROWSER_RUNTIME */

  let lib;
  if (typeof BROWSER_RUNTIME != "undefined" && BROWSER_RUNTIME) {
    lib = require("@emurgo/cardano-serialization-lib-browser");
  } else {
    lib = require("@emurgo/cardano-serialization-lib-nodejs");
  }
  lib = require("@mlabs-haskell/csl-gc-wrapper")(lib);

  exports.newPlutusV1Script = bytes => lib.PlutusScript.new(bytes);

  exports.newPlutusV2Script = bytes => lib.PlutusScript.new_v2(bytes);
})(PS["Ctl.Internal.Serialization.PlutusScript"] = PS["Ctl.Internal.Serialization.PlutusScript"] || {});
(function($PS) {
  // Generated by purs version 0.14.9
  "use strict";
  $PS["Ctl.Internal.Serialization.PlutusScript"] = $PS["Ctl.Internal.Serialization.PlutusScript"] || {};
  var exports = $PS["Ctl.Internal.Serialization.PlutusScript"];
  var $foreign = $PS["Ctl.Internal.Serialization.PlutusScript"];
  var Ctl_Internal_Types_Scripts = $PS["Ctl.Internal.Types.Scripts"];                
  var convertPlutusScript = function (v) {
      return (function () {
          if (v.value1 instanceof Ctl_Internal_Types_Scripts.PlutusV1) {
              return $foreign.newPlutusV1Script;
          };
          if (v.value1 instanceof Ctl_Internal_Types_Scripts.PlutusV2) {
              return $foreign.newPlutusV2Script;
          };
          throw new Error("Failed pattern match at Ctl.Internal.Serialization.PlutusScript (line 24, column 11 - line 26, column 36): " + [ v.value1.constructor.name ]);
      })()(v.value0);
  };
  exports["convertPlutusScript"] = convertPlutusScript;
})(PS);
(function($PS) {
  // Generated by purs version 0.14.9
  "use strict";
  $PS["Data.Either"] = $PS["Data.Either"] || {};
  var exports = $PS["Data.Either"];
  var Data_Functor = $PS["Data.Functor"];          
  var Left = (function () {
      function Left(value0) {
          this.value0 = value0;
      };
      Left.create = function (value0) {
          return new Left(value0);
      };
      return Left;
  })();
  var Right = (function () {
      function Right(value0) {
          this.value0 = value0;
      };
      Right.create = function (value0) {
          return new Right(value0);
      };
      return Right;
  })();
  var functorEither = {
      map: function (f) {
          return function (m) {
              if (m instanceof Left) {
                  return new Left(m.value0);
              };
              if (m instanceof Right) {
                  return new Right(f(m.value0));
              };
              throw new Error("Failed pattern match at Data.Either (line 31, column 1 - line 31, column 52): " + [ m.constructor.name ]);
          };
      }
  };
  var either = function (v) {
      return function (v1) {
          return function (v2) {
              if (v2 instanceof Left) {
                  return v(v2.value0);
              };
              if (v2 instanceof Right) {
                  return v1(v2.value0);
              };
              throw new Error("Failed pattern match at Data.Either (line 208, column 1 - line 208, column 64): " + [ v.constructor.name, v1.constructor.name, v2.constructor.name ]);
          };
      };
  };
  var applyEither = {
      apply: function (v) {
          return function (v1) {
              if (v instanceof Left) {
                  return new Left(v.value0);
              };
              if (v instanceof Right) {
                  return Data_Functor.map(functorEither)(v.value0)(v1);
              };
              throw new Error("Failed pattern match at Data.Either (line 70, column 1 - line 72, column 30): " + [ v.constructor.name, v1.constructor.name ]);
          };
      },
      Functor0: function () {
          return functorEither;
      }
  };
  var bindEither = {
      bind: either(function (e) {
          return function (v) {
              return new Left(e);
          };
      })(function (a) {
          return function (f) {
              return f(a);
          };
      }),
      Apply0: function () {
          return applyEither;
      }
  };
  exports["Left"] = Left;
  exports["Right"] = Right;
  exports["functorEither"] = functorEither;
  exports["bindEither"] = bindEither;
})(PS);
(function($PS) {
  // Generated by purs version 0.14.9
  "use strict";
  $PS["Data.Profunctor"] = $PS["Data.Profunctor"] || {};
  var exports = $PS["Data.Profunctor"];                  
  var profunctorFn = {
      dimap: function (a2b) {
          return function (c2d) {
              return function (b2c) {
                  return function ($8) {
                      return c2d(b2c(a2b($8)));
                  };
              };
          };
      }
  };
  exports["profunctorFn"] = profunctorFn;
})(PS);
(function($PS) {
  // Generated by purs version 0.14.9
  "use strict";
  $PS["Data.Profunctor.Choice"] = $PS["Data.Profunctor.Choice"] || {};
  var exports = $PS["Data.Profunctor.Choice"];
  var Data_Either = $PS["Data.Either"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Profunctor = $PS["Data.Profunctor"];
  var left = function (dict) {
      return dict.left;
  };
  var choiceFn = {
      left: function (v) {
          return function (v1) {
              if (v1 instanceof Data_Either.Left) {
                  return new Data_Either.Left(v(v1.value0));
              };
              if (v1 instanceof Data_Either.Right) {
                  return new Data_Either.Right(v1.value0);
              };
              throw new Error("Failed pattern match at Data.Profunctor.Choice (line 32, column 1 - line 35, column 16): " + [ v.constructor.name, v1.constructor.name ]);
          };
      },
      right: Data_Functor.map(Data_Either.functorEither),
      Profunctor0: function () {
          return Data_Profunctor.profunctorFn;
      }
  };
  exports["left"] = left;
  exports["choiceFn"] = choiceFn;
})(PS);
(function($PS) {
  // Generated by purs version 0.14.9
  "use strict";
  $PS["Ctl.Internal.ApplyArgs"] = $PS["Ctl.Internal.ApplyArgs"] || {};
  var exports = $PS["Ctl.Internal.ApplyArgs"];
  var $foreign = $PS["Ctl.Internal.ApplyArgs"];
  var Control_Bind = $PS["Control.Bind"];
  var Ctl_Internal_Deserialization_WitnessSet = $PS["Ctl.Internal.Deserialization.WitnessSet"];
  var Ctl_Internal_Serialization_PlutusData = $PS["Ctl.Internal.Serialization.PlutusData"];
  var Ctl_Internal_Serialization_PlutusScript = $PS["Ctl.Internal.Serialization.PlutusScript"];
  var Ctl_Internal_Types_PlutusData = $PS["Ctl.Internal.Types.PlutusData"];
  var Data_Either = $PS["Data.Either"];
  var Data_Profunctor_Choice = $PS["Data.Profunctor.Choice"];      
  var ApplyArgsError = function (x) {
      return x;
  };                                                      
  var apply_params_to_script_either = $foreign.apply_params_to_script(Data_Either.Left.create)(Data_Either.Right.create);
  var applyArgs = function (script) {
      return function (paramsList) {
          return Data_Profunctor_Choice.left(Data_Profunctor_Choice.choiceFn)(ApplyArgsError)((function () {
              var params = Ctl_Internal_Serialization_PlutusData.convertPlutusData(new Ctl_Internal_Types_PlutusData.List(paramsList));
              return Control_Bind.bind(Data_Either.bindEither)(apply_params_to_script_either(params)(Ctl_Internal_Serialization_PlutusScript.convertPlutusScript(script)))(function (appliedScript) {
                  return new Data_Either.Right(Ctl_Internal_Deserialization_WitnessSet.convertPlutusScript(appliedScript));
              });
          })());
      };
  };
  exports["applyArgs"] = applyArgs;
})(PS);
(function($PS) {
  // Generated by purs version 0.14.9
  "use strict";
  $PS["Contract.Scripts"] = $PS["Contract.Scripts"] || {};
  var exports = $PS["Contract.Scripts"];
  var Ctl_Internal_ApplyArgs = $PS["Ctl.Internal.ApplyArgs"];
  exports["applyArgs"] = Ctl_Internal_ApplyArgs.applyArgs;
})(PS);
(function($PS) {
  // Generated by purs version 0.14.9
  "use strict";
  $PS["MlabsPlutusTemplate.Api"] = $PS["MlabsPlutusTemplate.Api"] || {};
  var exports = $PS["MlabsPlutusTemplate.Api"];
  var Contract_Scripts = $PS["Contract.Scripts"];                
  var square = function (n) {
      return n * n | 0;
  };
  exports["square"] = square;
  exports["applyArgs"] = Contract_Scripts.applyArgs;
})(PS);
module.exports = PS["MlabsPlutusTemplate.Api"];
