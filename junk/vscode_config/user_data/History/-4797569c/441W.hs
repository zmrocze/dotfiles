import Playground.Contract
import Data.Text
import Data.List.NonEmpty (NonEmpty ((:|)))
import Ledger.Value (TokenName(TokenName))
import Ledger.Scripts (ValidatorHash (..))
import Playground.Types (KnownCurrency (..))
import PlutusTx.Prelude

myCurrency :: KnownCurrency
myCurrency = KnownCurrency (ValidatorHash \"\") \"MyCurrency\" (TokenName \"MyToken\" :| [])
$(mkKnownCurrencies ['myCurrency])

schemas = []
iotsDefinitions = \"\"