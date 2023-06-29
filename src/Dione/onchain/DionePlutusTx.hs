{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module DionePlutusTx where

import           Plutus.V1.Ledger.Interval (contains)
import           Plutus.V2.Ledger.Api      (BuiltinData, POSIXTime, PubKeyHash,
                                            ScriptContext (scriptContextTxInfo),
                                            TxInfo (txInfoValidRange),
                                            Validator, from, mkValidatorScript)
import           Plutus.V2.Ledger.Contexts (txSignedBy)
import           PlutusTx                  (applyCode, compile, liftCode,
                                            makeLift)
import           PlutusTx.Prelude          (Bool (True,False), traceIfFalse, Integer, ($), (&&), (.))
import           Prelude                   (IO)
import           Utilities                 (wrapValidator, writeValidatorToFile)

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

data DioneParams = DioneParams
    { contractSalt :: Integer
    }
makeLift ''DioneParams

{-# INLINABLE mkParameterizedAlwaysSucceeds #-}
mkParameterizedAlwaysSucceeds :: DioneParams -> () -> () -> ScriptContext -> Bool
mkParameterizedAlwaysSucceeds _ _ _ _ = True

{-# INLINABLE  mkWrappedParameterizedAlwaysSucceeds #-}
mkWrappedParameterizedAlwaysSucceeds :: DioneParams -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedParameterizedAlwaysSucceeds = wrapValidator . mkParameterizedAlwaysSucceeds

parameterizedValidator :: DioneParams -> Validator
parameterizedValidator dParams = mkValidatorScript ($$(compile [|| mkWrappedParameterizedAlwaysSucceeds ||]) `applyCode` liftCode dParams)

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

saveVal :: DioneParams -> IO ()
saveVal = writeValidatorToFile "./assets/lockingcontract-v0.1.plutus" . parameterizedValidator
