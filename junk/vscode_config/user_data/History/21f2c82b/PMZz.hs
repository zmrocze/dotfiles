{-# LANGUAGE OverloadedStrings #-}

module Test.Plutip.Internal.BotPlutusInterface.Keys (
  KeyPair (sKey, vKey),
  StakeKeyPair (sSKey, sVKey),
  genKeyPair,
  writeKeyPair,
  genStakeKeyPair,
  writeStakeKeyPairs,
  signingKeyFilePathInDir,
  verificationKeyFilePathInDir,
  stakingSigningKeyFilePathInDir,
  stakingVerificationKeyFilePathInDir,
) where

import Cardano.Api (writeFileTextEnvelope)
import Cardano.Api qualified as CAPI
import System.FilePath ((<.>), (</>))
import Data.Text (Text)
import Data.String (IsString(fromString))
import qualified Data.Text as Text

data KeyPair = KeyPair
  { sKey :: CAPI.SigningKey CAPI.PaymentKey
  , vKey :: CAPI.VerificationKey CAPI.PaymentKey
  }
  deriving stock (Show)

data StakeKeyPair = StakeKeyPair
  { sSKey :: CAPI.SigningKey CAPI.StakeKey
  , sVKey :: CAPI.VerificationKey CAPI.StakeKey
  }
  deriving stock (Show)

rmQuotes :: String -> String
rmQuotes = filter (/= '"')

pSKeyDesc, pVKeyDesc, sSKeyDesc, sVKeyDesc :: CAPI.TextEnvelopeDescr
pSKeyDesc = "Payment Signing Key"
pVKeyDesc = "Payment Verification Key"
sSKeyDesc = "Delegation Signing Key"
sVKeyDesc = "Delegation Verification Key"

-- | Helper to generate key pairs.
-- Can be further developed to generate test keys for test wallets
-- to work with `bot-plutus-interface`
genKeyPair :: IO KeyPair
genKeyPair = do
  sKey <- CAPI.generateSigningKey CAPI.AsPaymentKey
  return $ KeyPair sKey (CAPI.getVerificationKey sKey)

writeKeyPair :: FilePath -> KeyPair -> IO [Either (CAPI.FileError ()) ()]
writeKeyPair outDir keyPair = do
  let hash = rmQuotes . show $ CAPI.verificationKeyHash $ vKey keyPair

      skeyPath = outDir hash  "skey"
      vkeyPath = outDir hash  "vkey"

  sequence
    [ writeFileTextEnvelope skeyPath (Just pSKeyDesc) (sKey keyPair)
    , writeFileTextEnvelope vkeyPath (Just pVKeyDesc) (vKey keyPair)
    ]

genStakeKeyPair :: IO StakeKeyPair
genStakeKeyPair = do
  sKey <- CAPI.generateSigningKey CAPI.AsStakeKey
  return $ StakeKeyPair sKey (CAPI.getVerificationKey sKey)

writeStakeKeyPairs :: FilePath -> StakeKeyPair -> IO [Either (CAPI.FileError ()) ()]
writeStakeKeyPairs dir stakeKeyPair = do
  let hash = CAPI.verificationKeyHash $ sVKey stakeKeyPair

      skeyPath = stakingSigningKeyFilePathInDir dir hash
      vkeyPath = stakingVerificationKeyFilePathInDir dir hash

  sequence
    [ writeFileTextEnvelope (show skeyPath) (Just sSKeyDesc) (sSKey stakeKeyPair)
    , writeFileTextEnvelope vkeyPath (Just sVKeyDesc) (sVKey stakeKeyPair)
    ]

signingKeyFilePathInDir :: FilePath -> CAPI.Hash CAPI.PaymentKey -> FilePath
signingKeyFilePathInDir dir vkh = keyFilePathInDir dir "signing-key" vkh "skey"

verificationKeyFilePathInDir :: FilePath -> CAPI.Hash CAPI.PaymentKey -> FilePath
verificationKeyFilePathInDir dir vkh = keyFilePathInDir dir "verification-key" vkh "vkey"

stakingSigningKeyFilePathInDir :: FilePath -> CAPI.Hash CAPI.StakeKey -> FilePath
stakingSigningKeyFilePathInDir dir vkh = keyFilePathInDir dir "staking-signing-key" vkh "skey"

stakingVerificationKeyFilePathInDir :: FilePath -> CAPI.Hash CAPI.StakeKey -> FilePath
stakingVerificationKeyFilePathInDir dir vkh = keyFilePathInDir dir "staking-verification-key" vkh "vkey"

keyFilePathInDir :: forall a . CAPI.SerialiseAsRawBytes (CAPI.Hash a) => FilePath -> String -> CAPI.Hash a -> String -> FilePath
keyFilePathInDir dir pref h ext = dir </> pref <> "-" <> Text.unpack (CAPI.serialiseToRawBytesHexText h) <.> ext
