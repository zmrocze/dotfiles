{-# LANGUAGE OverloadedStrings  #-}

module Main where

import qualified Codec.Serialise as CBOR
import Data.ByteString.Lazy (fromStrict)
import qualified Data.ByteString as B
-- import Data.ByteString.Short
import Data.Text (Text)
import Plutus.V2.Ledger.Api

bs :: B.ByteString
bs = "Y\b\SI\SOH\NUL\NUL22223\"2222222223\"22222#\"222##%3S###P\STX\"#%3P\SOH\DLE\"\DC35s\137!\SYNsignature check failed\NUL\STX\DC33W4fn\208\NUL@\f\NUL\128\132\b\f\204\213\205\EM\184sUs\170\NUL\137\NUL\SOH\EM\145\t\EM\128\b\SOH\128\DC1\145\145\145\145\145\145\145\145\145\145\145\145\145\153\154\185\163\&7\SOj\174u@1 \NUL#333332\"\"\"\"\"!#333330\SOH\NUL\208\f\NUL\176\n\NUL\144\b\NULp\ACK\NULP\EOT\NUL0\STX3P\ESC\SOH\195WB\160\CANf\160\&6\ETX\134\174\133@,\205@l\aM]\n\128Q\153\170\128\251\174P\RS5t*\SOH&f\170\ETX\238\185@x\213\208\168\EOT\EM\168\r\129\&1\171\161P\a35P\US\STXwZj\232T\SOH\140\140\140\140\204\213\205\EM\184sUs\170\NULI\NUL\SOH\EM\145\t\EM\128\b\SOH\128\DC1\145\145\145\153\154\185\163\&7\SOj\174u@\t \NUL#2!#0\SOH\NUL0\STX3P1u\166\174\133@\b\192\200\213\208\154\186%\NUL\"2c 63W8\ACK\224l\ACK\130j\174y@\EOTM\213\NUL\t\171\161P\STX22233W4f\225\205U\206\168\SOH$\NUL\EOTfD$f\NUL \ACK\NULFj\ACK.\180\213\208\168\SOH\CAN\EM\SUB\186\DC3WD\160\EOTFLd\ACK\198j\231\NUL\220\r\128\208MU\207(\NUL\137\186\160\SOH5t&\174\137@\b\140\152\200\f\140\213\206\SOH\152\EM\SOH\128\154\171\158P\SOH\DC3u@\STXj\232T\SOHL\212\ACK\221q\171\161P\EOT35P\US\STX2\NUL\DC3WB\160\ACKfj\160>\235\136\NULM]\n\128\DC1\129)\171\161\&5tJ\NULDd\198@\\f\174p\v\192\184\v\EOT\213\209(\NUL\137\171\162P\SOH\DC3WD\160\STX&\174\137@\EOTM]\DC2\128\b\154\186%\NUL\DC15tJ\NUL\"j\232\148\NULD\213\209(\NUL\137\171\162P\SOH\DC3Us\202\NUL\"n\168\NULM]\n\128!\128\169\171\161\&5tJ\NUL\132d\198@@f\174p\b@\128\a\140\204\213\205\EM\184sUs\170\NUL\233\NUL\SOH\EM\153\DC1\t\EM\152\NUL\128 \SOH\128\DC1\186\227WB\160\SOn\184\213\208\168\ETX\ESC\174\&5t&\174\137@\CAN\140\152\200\a\204\213\206\SOH\NUL\SI\128\233\153\154\185\163\&7\SOj\174u@! \NUL#u\166\174\132\213\\\242\128I\EM1\144\SI\EM\171\156\SOH\240\RS\SOH\193\SOH\209\&2c \GS3W8\146\SOH\ETXPT5\NUL\SOH\209\&5W<\160\STX&\234\128\EOTM]\DC2\128\b\154\171\158P\SOH\DC3u@\STX&\234\128\EOTH\200\140\NUL\141\214\NUL\t\144\NUL\154\168\v\DC1\EM\153\170\185\240\SOH%\NUL\162\&3P\t0\EOT5t \EOT`\ACKj\232\128\b\ENQ\136\200\200\204\205\\\209\155\135\&5W:\160\EOT\144\NUL\DC1\153\DLE\145\152\NUL\128\CAN\SOH\CAN\ACK\SUB\186\NAK\NUL#\NULSWBj\232\148\NUL\136\201\140\128X\205\\\224\v\128\176\n\t\170\185\229\NUL\DC17T\NUL$ddddff\174h\205\195\154\171\157P\EOTH\NUL\b\204\204\136\136H\204\204\NUL@\DC4\SOH\NUL\f\NUL\140\140\140\140\204\213\205\EM\184sUs\170\NULI\NUL\SOH\EM\145\t\EM\128\b\SOH\128\DC1\128\169\171\161P\STX3P\SI\SOHCWBj\232\148\NUL\136\201\140\128l\205\\\224\SO\NUL\216\f\137\170\185\229\NUL\DC17T\NUL&\174\133@\DLE\204\213@!\215(\ETX\154\186\NAK\NUL3###35sFn\GS@\ENQ \EOT#!\"#\NUL \EOT5t&\170\231\148\NUL\200\204\205\\\209\155\135P\STXH\NUL\136\200H\136\192\EOT\SOH\r\215\SUB\186\DC3Us\202\NUL\132ff\174h\205\195\168\SOH\164\NUL\EOT$D\NULdd\198@:f\174p\a\128t\ACK\192h\ACKD\213\\\234\128\b\155\170\NUL\DC3WB\160\EOTf\160\SYN\235\141]\t\171\162P\STX#&2\SOHs5s\128\&0\STX\224*&\174\137@\EOTM]\DC2\128\b\154\171\158P\SOH\DC3u@\STX&j\160\STX\235\157h\137\DC1\145\CAN\SOH\ESC\171\NUL\DC3 \SOH5P\DC3\"233Us\224\EOTJ\SOH\EOTf\160\SOfD$f\NUL \ACK\NULF\NUL\198\170\231T\NUL\140\SOHMU\207(\SOH\CAN\STX\SUB\186 \ETX\SOHA5t \STX\"D\NULBD$F`\STX\NUL\128\ACK$FFFfj\230\140\220:\128\n@\NULFBD`\EOT\NULf\NUL\166\174\132\213\\\242\128\EM\EM\153\171\154\&3p\234\NULI\NUL\DC1\t\DLE\NUL\145\147\EM\NUL\145\154\185\192\DC3\SOH \DLE\NUL\241\&5W:\160\STX&\234\128\EOT\140\140\140\204\213\205\EM\184u\NUL\DC4\128\CAN\140\132\136\136\192\DLE\SOHL\SOH\205]\t\170\185\229\NUL233W4f\225\212\NUL\146\NULB2\DC2\"#\NUL \ENQ0\t5t&\170\231\148\SOH\b\204\205\\\209\155\135P\ETXH\NUL\136\200H\136\140\NUL@\DC4\192\FS\213\208\154\171\158P\ENQ#35sFn\GS@\DC1 \NUL#!\"\"0\ETX\NULSu\198\174\132\213\\\242\128\&1\EM1\144\t\EM\171\156\SOH0\DC2\SOH\NUL\SI\NUL\224\r\DC3Us\170\NUL\"n\168\NULH\200\200\204\205\\\209\155\135\&5W:\160\EOT\144\NUL\DC1\153\DLE\145\152\NUL\128\CAN\SOH\CAN\STX\154\186\NAK\NUL#u\166\174\132\213\209(\SOH\DC1\147\EM\NULq\154\185\192\SI\NUL\224\f\DC3Us\202\NUL\"n\168\NULH\200\204\205\\\209\155\135\&5W:\160\STX\144\NUL\DC1\186\227WBj\174y@\b\140\152\200\ETX\f\213\206\NULh\ACK\NULP\155\170\NUL\DC22222233W4f\225\212\NULR\NUL\194\DC2\"\"\"\NUL233W4f\225\212\NUL\146\NUL\162\DC2\"\"\"\NULB33W4f\225\212\NUL\210\NUL\130\&3\"\DC2\"\"\"3\NUL\DLE\t\NUL\131u\198\174\133@\DC4\221i\171\161\&5tJ\NUL\164ff\174h\205\195\168\STX$\NUL\196fD$DDDf\NUL@\DC2\SOH\ACK\235\141]\n\128\&9\186\227WBj\232\148\SOH\200\204\205\\\209\155\135P\ENQH\SOH\b\204\136H\136\136\136\204\SOH\128$\STX\f\ETX\r]\n\128I\186\227WBj\232\148\STXH\204\205\\\209\155\135P\ACKH\NUL\136\200H\136\136\136\192\FS\STX\f\ETXM]\t\170\185\229\NUL\178\&33W4f\225\212\SOH\210\NUL\STX2\DC2\"\"\"0\ENQ\NUL\131\NUL\227WBj\174y@0\140\152\200\ENQL\213\206\NUL\176\n\128\152\t\NUL\136\b\NULx\a\NULh\154\171\157P\EOT\DC3Us\202\NULbj\174y@\bMU\207(\NUL\137\186\160\SOH#####35sFn\GS@\ENQ \STX#3\"!\"30\SOH\NULP\EOT\NUL3u\166\174\133@\DLE\221i\171\161P\ETX7Zj\232M]\DC2\128\EM\EM\153\171\154\&3p\234\NULI\NUL\SOH\EM\t\DC1\128\DLE\SOH\152\EOT\SUB\186\DC3Us\202\NUL\196d\198@\FSf\174p\ETX\192\&8\ETX\NUL,MU\206\168\SOH\137\171\162P\SOH\DC3Us\202\NUL\"n\168\NULH\200\200\204\205\\\209\155\135P\SOHH\NUL\136\200H\140\NUL@\f\221q\171\161\&5W<\160\ACKFfj\230\140\220:\128\DC2@\NULFBD`\EOT\NULf\235\141]\t\170\185\229\NULB2c \v3W8\SOH\128\SYN\SOH \DLE&\170\231T\NULD\221P\NUL\137\DC1\145\145\153\154\185\163\&7\SO\160\STX\144\STX\DLE\145\DLE\NUL\145\153\154\185\163\&7\SO\160\EOT\144\SOH\DC1\144\145\DC1\128\CAN\STX\CAN\ETX\SUB\186\DC3Us\202\NUL\132ff\174h\205\195\168\SOH\164\NUL\EOT$D\NULDd\198@\CANf\174p\ETX@0\STX\128$\STX\EOT\213\\\234\128\b\155\170\NUL\DC2233W4f\225\212\NULR\NUL\"\NULR33W4f\225\212\NUL\146\NUL\STX\NULR2c \b3W8\SOH \DLE\NUL\192\n&\170\231M\213\NUL\b\145\NUL\DLE\145\NUL\nL$\NUL)!\ETXPT1\NUL\DC1##\NUL\DLE\SOH\"3\NUL3\NUL \STX\NUL\DC1"

res :: Either CBOR.DeserialiseFailure Script
res = CBOR.deserialiseOrFail $ fromStrict $ bs

script = fromRight undefined res

main = print res