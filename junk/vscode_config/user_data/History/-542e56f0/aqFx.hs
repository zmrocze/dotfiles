
module Test.Plutip.Internal.Keys (KeyPair, cardanoMainnetAddress, genKeyPair, mainnetAddress) where 

import Cardano.Api (AsType (AsPaymentKey), Key (VerificationKey, getVerificationKey, verificationKeyHash), PaymentKey, SigningKey, TextEnvelopeDescr, generateSigningKey, writeFileTextEnvelope, AddressAny)
import System.FilePath ((<.>), (</>))
import qualified Cardano.Api as CAPI
import qualified Data.Text as Text

data KeyPair = KeyPair
  { sKey :: SigningKey PaymentKey
  , vKey :: VerificationKey PaymentKey
  }
  deriving stock (Show)

genKeyPair :: IO KeyPair
genKeyPair = do
  sKey <- generateSigningKey AsPaymentKey
  return $ KeyPair sKey (getVerificationKey sKey)

-- | Make `AnyAddress` for mainnet
cardanoMainnetAddress :: KeyPair -> AddressAny
cardanoMainnetAddress KeyPair {vKey} =
  CAPI.toAddressAny $
    CAPI.makeShelleyAddress
      CAPI.Mainnet
      (CAPI.PaymentCredentialByKey (CAPI.verificationKeyHash vKey))
      CAPI.NoStakeAddress

-- | Get `String` representation of address on mainnet
mainnetAddress :: KeyPair -> String
mainnetAddress =
  showAddress . cardanoMainnetAddress

showAddress :: AddressAny -> String
showAddress = Text.unpack . CAPI.serialiseAddress

saveKeyPair :: KeyPair -> FilePath -> IO ()

saveWallets :: KeyPair -> Maybe FilePath -> ReaderT ClusterEnv m (Either BpiError ())
saveWallets bpiw fp = do
  cEnv <- ask
  case fp of
    Nothing -> pure ()
    (Just wdir) -> void $ saveWalletDir bpiw wdir
  saveWalletDir bpiw (Setup.keysDir cEnv)

-- | Save the wallet to a specific directory.
saveWalletDir :: MonadIO m => BpiWallet -> FilePath -> m (Either BpiError ())
saveWalletDir (BpiWallet pkh _ sk) wallDir = do
  liftIO $ createDirectoryIfMissing True wallDir
  let pkhStr = Text.unpack (encodeByteString (fromBuiltin (LAPI.getPubKeyHash pkh)))
      path = wallDir </> "signing-key-" ++ pkhStr <.> "skey"
  res <- liftIO $ CAPI.writeFileTextEnvelope path (Just "Payment Signing Key") sk
  return $ left (SignKeySaveError . show) res --todo: better error handling


genKeyPairs :: FilePath -> String -> String -> IO ()
genKeyPairs outDir sKeyPrefix vKeyPrefix = do
  let skeyDesc, vkeyDesc :: TextEnvelopeDescr
      skeyDesc = "Payment Signing Key"
      vkeyDesc = "Payment Verification Key"

      vKey = getVerificationKey sKey
      hash = verificationKeyHash vKey

      skeyPath = rmQuotes $ outDir </> sKeyPrefix ++ showHash hash <.> "skey"
      vkeyPath = rmQuotes $ outDir </> vKeyPrefix ++ showHash hash <.> "vkey"

      showHash = rmQuotes . show
  res <-
    sequence
      [ writeFileTextEnvelope skeyPath (Just skeyDesc) sKey
      , writeFileTextEnvelope vkeyPath (Just vkeyDesc) vKey
      ]
  print res

rmQuotes :: String -> String
rmQuotes = filter (/= '"')
