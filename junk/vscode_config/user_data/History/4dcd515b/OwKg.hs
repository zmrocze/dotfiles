module Hsh.Constructors where

data Allowed
  = JustIO     (IO ())
  | Func  (Arg Val)
  | Action (Arg (IO Val))
  | IOFunc (Arg (IO ()))

data Arg v
  = InStr (String -> v)     
  | Lines     ([String] -> v)
  | WordLines  ([[String]] -> v)

data Val
  = OutStr String
  | UnLines [String]
  | UnWordLines [[String]]
