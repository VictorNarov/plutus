import Playground.Contract
import           Control.Monad             (void)
import           Data.Aeson                (FromJSON, ToJSON)
import qualified Data.Text                 as T
import           GHC.Generics              (Generic)
import           Language.Plutus.Contract
import qualified Language.PlutusTx         as PlutusTx
import           Language.PlutusTx.Prelude
import           Ledger
import qualified Ledger.Ada                as Ada
import qualified Ledger.Constraints        as Constraints
import qualified Ledger.Typed.Scripts      as Scripts
import           Schema
import           Wallet.Emulator.Wallet

data SplitData =
    SplitData
        { recipient :: PubKeyHash -- ^ Second recipient of the funds
        , amount     :: Ada -- ^ How much Ada we want to lock
        }
    deriving stock (Show, Generic)

PlutusTx.makeIsData ''SplitData
PlutusTx.makeLift ''SplitData

validateSplit :: SplitData -> () -> ValidatorCtx -> Bool
validateSplit _ _ _ = True

data Split
instance Scripts.ScriptType Split where
    type instance RedeemerType Split = ()
    type instance DatumType Split = SplitData

splitInstance :: Scripts.ScriptInstance Split
splitInstance = Scripts.validator @Split
    $$(PlutusTx.compile [|| validateSplit ||])
    $$(PlutusTx.compile [|| wrap ||]) where
        wrap = Scripts.wrapValidator @SplitData @()

data LockArgs =
        LockArgs
            { recipientWallet :: Wallet
            , totalAda         :: Ada
            }
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

type SplitSchema =
    BlockchainActions
        .\/ Endpoint "lock" LockArgs
        .\/ Endpoint "unlock" LockArgs

lock :: Contract SplitSchema T.Text LockArgs
lock = endpoint @"lock"

unlock :: Contract SplitSchema T.Text LockArgs
unlock = endpoint @"unlock"

mkSplitData :: LockArgs -> SplitData
mkSplitData LockArgs{recipientWallet, totalAda} =
    let convert :: Wallet -> PubKeyHash
        convert = pubKeyHash . walletPubKey
    in
    SplitData
        { recipient = convert recipientWallet
        , amount = totalAda
        }

lockFunds :: SplitData -> Contract SplitSchema T.Text ()
lockFunds s@SplitData{amount} = do
    logInfo $ "Locking " <> show amount
    let tx = Constraints.mustPayToTheScript s (Ada.toValue amount)
    void $ submitTxConstraints splitInstance tx

unlockFunds :: SplitData -> Contract SplitSchema T.Text ()
unlockFunds SplitData{recipient, amount} = do
    let contractAddress = (Ledger.scriptAddress (Scripts.validatorScript splitInstance))
    utxos <- utxoAt contractAddress
    let tx =
            collectFromScript utxos ()
            <> Constraints.mustPayToPubKey recipient (Ada.toValue $ amount)
    void $ submitTxConstraintsSpending splitInstance utxos tx

endpoints :: Contract SplitSchema T.Text ()
endpoints = (lock >>= lockFunds . mkSplitData) `select` (unlock >>= unlockFunds . mkSplitData)

mkSchemaDefinitions ''SplitSchema
$(mkKnownCurrencies [])

