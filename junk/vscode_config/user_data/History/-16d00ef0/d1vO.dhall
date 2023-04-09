{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "ctl-package-example"
, dependencies =
  [ "aff"
  , "argonaut"
  , "arrays"
  , "bigints"
  , "cardano-transaction-lib"
  , "datetime"
  , "effect"
  , "exceptions"
  , "foreign-object"
  , "mote"
  , "node-buffer"
  , "node-fs-aff"
  , "ordered-collections"
  , "posix-types"
  , "prelude"
  , "profunctor"
  , "spec"
  , "uint"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "exe/**/*.purs", "test/**/*.purs" ]
}
