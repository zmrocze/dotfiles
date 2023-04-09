module Test.ClusterBug where

import Ctl.Internal.Plutip.Server (checkPlutipServer)
import Test.Ctl.Plutip.Common (config)

main = launchAff_ $ checkPlutipServer config
