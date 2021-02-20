# Smart Contract con Plutus
:office: Universidad de Huelva (UHU)  
:calendar: Curso 2020-2021  
:mortar_board: Modelos Avanzados de Computación  
:octocat: [Ihar Myshkevich (@IgorMy)](https://github.com/IgorMy)  
:octocat: [Víctor M. Rodríguez (@VictorNarov)](https://github.com/VictorNarov)  

## Introducción
En este repositorio se documentará la implementación de un Smart Contract codificado en lenguaje Haskell sobre plataforma Plutus Playground. 
En este, se visualiza cómo una cartera genera un contrato de transferencia de capital (Ada) y cómo la carpeta destinataria recoge ese capital. 
<p align="center">
  <img width="500" height="300" src="images/plutus.png">
</p>  

## Partes del Smart Contract
En este apartado veremos las diferentes partes de este Smart Contract.

### Librerías
```
import Playground.Contract                                     -- Gestión de contratos en el entorno Plutus Playground.
import           Control.Monad             (void)              -- Funciónes de cálculo avanzado.
import           Data.Aeson                (FromJSON, ToJSON)  -- Tipos y funciones para trabajar eficazmente con datos JSON.
import qualified Data.Text                 as T                -- Tipos y funciones para trabajar eficazmente con texto plano.
import           GHC.Generics              (Generic)           -- Funciones para la conversión de datos.
import           Language.Plutus.Contract                      -- Contratos Pulutus.
import qualified Language.PlutusTx         as PlutusTx         -- Bibliotecas y el compilador para compilar Haskell en Plutus.
import           Language.PlutusTx.Prelude                     -- Sustituto del Haskell prelude que funciona mejor con PlutusTx.
import           Ledger                                        -- Contenedor para almacenar datos en bruto de una forma más eficiente.
import qualified Ledger.Ada                as Ada              -- Almacenamiento de tipo Ada (moneda).
import qualified Ledger.Constraints        as Constraints      -- Restricciones del almacenamiento.
import qualified Ledger.Typed.Scripts      as Scripts          -- Funciones del almacenamiento.
import           Schema                                        -- Bilioteca Haskell para serializar y deserializar datos en JSON.
import           Wallet.Emulator.Wallet                        -- Biblioteca Haskell para gestionar carteras virtuales.
```
En este apatado importamos las librerías de CARDANO necesarias para la ejecución del Smart Contract. Su funcionalidad está comentada a la derecha de su declaración.

### Definición de tipo de datos
```
data SmartContractData =
    SmartContractData
        { recipient :: PubKeyHash 
        , amount     :: Ada
        }
    deriving stock (Show, Generic)

PlutusTx.makeIsData ''SmartContractData
PlutusTx.makeLift ''SmartContractData
```
SmartContractData describe el destinatario al que se le va a enviar el capital y la cantidad de capital en Ada.
Estamos utilizando el tipo PubKeyHash para identificar al destinatario. Al realizar el pago podemos utilizar el hash para crear la salida de clave pública.

### Script de validación
```
validateSmartContract :: SmartContractData -> () -> ValidatorCtx -> Bool
validateSmartContract SmartContractData{recipient, amount} _ ValidatorCtx{valCtxTxInfo} =
    Ada.fromValue (valuePaidTo valCtxTxInfo recipient) == amount
```    
Esta función es muy importante. Su misión es tomar ambas transacciones por separado y decidir si son válidas. Solo en ese caso se ejecuta y se cierra el contrato.
En nuestro caso, este script comprueba que la cantidad que recibirá el destinatario es la acordada por ambas partes.

### Recoger peticiones
```
data LockArgs =
        LockArgs
            { recipientWallet :: Wallet -- Cartera del destinatario
            , totalAda         :: Ada   -- Cantidad (Ada) a vincular al contrato
            }
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

type SmartContractSchema =
    BlockchainActions
        .\/ Endpoint "lock" LockArgs
        .\/ Endpoint "unlock" LockArgs
```
Para recoger las peticiones de los usuarios neceistamos declarar los "endpoints" correspondientes como parte del programa. El conjunto de todos los endpoints se denomina "schema". Lo construiremos usando el tipo Endpoint y el operador ```.\/``` para combinarlos.
Previamente hemos definido los parámetros necesarios para los endpoints, que son la dirección de la cartera destinataria y el cantidad de Ada a vincular con el contrato.  
Se han desarrollado dos acciones para interactuar con el contrato *lock* y *unlock*.

```
lock :: Contract SmartContractSchema T.Text LockArgs
lock = endpoint @"lock"

unlock :: Contract SmartContractSchema T.Text LockArgs
unlock = endpoint @"unlock"
```

Endpoint recibe como argumento el nombre como tipo Haskell usando el operando ```@```. En general el algoritmo de unificación de Haskell es lo suficientemente robusto para los tipos de argumentos pasados a una función. Pero hay algunos casos, como este, en los que es necesario indicarle el tipo de dato.

###### Función de transformación de datos
```
mkSmartContractData :: LockArgs -> SmartContractData
mkSmartContractData LockArgs{recipientWallet, totalAda} =
    let convert :: Wallet -> PubKeyHash
        convert = pubKeyHash . walletPubKey
    in
    SmartContractData
        { recipient = convert recipientWallet
        , amount = totalAda
        }
```
Para realizar correctamente la transferencia de un tipo wallet a otro, es necesario un objeto pubKeyHash. Es necesario convertir el valor del wallet a su Hash de clave pública.

### Vincular dinero al contrato
```
lockFunds :: SmartContractData -> Contract SmartContractSchema T.Text ()
lockFunds s@SmartContractData{amount} = do
    logInfo $ "Locking " <> show amount -- Muestra la cantidad de Ada vinculada al contrato por el registro
    let tx = Constraints.mustPayToTheScript s (Ada.toValue amount)
    void $ submitTxConstraints smartContractInstance tx
```
Con esta función vinculamos la cantidad obtenida del SmartContractData del usuario que inició el contrato.

### Obtener dinero del contrato
```
unlockFunds :: SmartContractData -> Contract SmartContractSchema T.Text ()
unlockFunds SmartContractData{recipient, amount} = do
    let contractAddress = (Ledger.scriptAddress (Scripts.validatorScript smartContractInstance))
    utxos <- utxoAt contractAddress
    let tx =
            collectFromScript utxos ()
            <> Constraints.mustPayToPubKey recipient (Ada.toValue $ amount)
    void $ submitTxConstraintsSpending smartContractInstance utxos tx
```
Mediante esta función el destinatario puede recibir el dinero que se ha depositado en el contrato. Gracias a la restricción ```mustPayToPubKey``` con destinatario ```recipient``` y la cantidad de capital ```amount```.  
El script de validación verificará que el destinatario haya solicitado la cantidad exacta vinculada al contrato por el remitente.

### Interfaz de Plutus Playground
```
endpoints :: Contract SmartContractSchema T.Text ()
endpoints = (lock >>= lockFunds . mkSmartContractData) `select` (unlock >>= unlockFunds . mkSmartContractData)

mkSchemaDefinitions ''SmartContractSchema
$(mkKnownCurrencies [])
```
Estas últimas lineas definen nuestra aplicación para que se pueda ejecutar en Plutus Playground. La función select ofrece dos ramificaciones para las carteras. Por un lado permite transferir el capital al contrato ejecutando la función ```lockFunds``` y por otra, recibir el capital del contrato con la función ```unlockFunds```. Asegura que una cartera solo pueda interactuar con una parte del contrato (enviar o recibir) y sea otra la que complemente el contrato.

### Simulación
<p align="center">
  <img width="1280" height="720" src="images/simulación.gif">
</p> 

## Bibliografia
- [Plutus Playground](https://playground.plutus.iohkdev.io/)
- [Documentación de Plutus](https://playground.plutus.iohkdev.io/tutorial/index.html)
- [Video ilustrativo del funcionamiento por Clio.1](https://www.youtube.com/watch?v=yQYXfDG63WI&t=3s)
