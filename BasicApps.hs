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
        { recipient1 :: PubKeyHash -- ^ First recipient of the funds
        , recipient2 :: PubKeyHash -- ^ Second recipient of the funds
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
            { recipient1Wallet :: Wallet
            , recipient2Wallet :: Wallet
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
mkSplitData LockArgs{recipient1Wallet, recipient2Wallet, totalAda} =
    let convert :: Wallet -> PubKeyHash
        convert = pubKeyHash . walletPubKey
    in
    SplitData
        { recipient1 = convert recipient1Wallet
        , recipient2 = convert recipient2Wallet
        , amount = totalAda
        }

lockFunds :: SplitData -> Contract SplitSchema T.Text ()
lockFunds s@SplitData{amount} = do
    logInfo $ "Locking " <> show amount
    let tx = Constraints.mustPayToTheScript s (Ada.toValue amount)
    void $ submitTxConstraints splitInstance tx

unlockFunds :: SplitData -> Contract SplitSchema T.Text ()
unlockFunds SplitData{recipient1, recipient2, amount} = do
    let contractAddress = (Ledger.scriptAddress (Scripts.validatorScript splitInstance))
    utxos <- utxoAt contractAddress
    let tx =
            collectFromScript utxos ()
            <> Constraints.mustPayToPubKey recipient2 (Ada.toValue $ amount)
    void $ submitTxConstraintsSpending splitInstance utxos tx

endpoints :: Contract SplitSchema T.Text ()
endpoints = (lock >>= lockFunds . mkSplitData) 

mkSchemaDefinitions ''SplitSchema
$(mkKnownCurrencies [])

