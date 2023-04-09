{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Network.Wai
import Network.Wai.Handler.Warp (run)
import GHC.Natural (Natural)
import Data.Text ( Text )
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Servant
import Control.Concurrent (MVar)
import Control.Concurrent.STM ()
import Control.Monad.STM (atomically)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.STM.TVar (modifyTVar', TVar)

type GetApi = "query" :> QueryParam "key" Text :> Get '[PlainText] Natural
type PostApi = "input" :> ReqBody '[PlainText] Text :> PostNoContent

type Api = PostApi :<|> GetApi

api :: Proxy Api
api = Proxy

server :: TVar CountMap -> Server Api
server var = handlePost var :<|> handleGet var
  where 

    handlePost :: TVar CountMap -> Text -> Handler ()
    handlePost var k = liftIO . atomically $ modifyTVar' var (countString k)

    handleGet ::  TVar CountMap -> Maybe Text -> Handler Natural
    handleGet var = _

app :: TVar CountMap -> Application
app var = serve api (server var)

main :: IO ()
main = do 
  let var = _
  run 9000 (app var)

-- server :: Application
-- server req respond = do 
--     case requestMethod req of 
--         mtd | mtd == methodGet -> handleGet
--     _

newtype CountMap = CountMap { getCountMap :: Map Text Natural }

countString :: Text -> CountMap -> CountMap
countString k (CountMap m) = CountMap $ M.alter (Just . maybe 1 (+ 1)) k m

getCount :: Text -> CountMap -> Natural
getCount k (CountMap m) = fromMaybe 0 $ M.lookup k m

