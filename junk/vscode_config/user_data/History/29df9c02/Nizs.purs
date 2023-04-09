module MLabsPlutusTemplate.TestBundler where

import Prelude

import CompiledScripts (scripts)
import Contract.Prelude (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
    log "Hello world"