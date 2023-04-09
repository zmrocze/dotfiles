module MLabsPlutusTemplate.TestBundler where

import Prelude

import CompiledScripts (scripts)
import Contract.Prelude (Effect)

main :: Effect Unit
main = do
    log "Hello world"