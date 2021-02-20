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
Split data describe el destinatario al que se le va a enviar el capital y la cantidad de capital en Ada.
Estamos utilizando el tipo PubKeyHash para identificar al destinatario. Al realizar el pago podemos utilizar el hash para crear la salida de clave pública.

### Script de validación
```
validateSplit :: SmartContractData -> () -> ValidatorCtx -> Bool
validateSplit SmartContractData{recipient, amount} _ ValidatorCtx{valCtxTxInfo} =
    Ada.fromValue (valuePaidTo valCtxTxInfo recipient) >= amount
```    
Esta función es muy importante. Su misión es tomar ambas transacciones por separado y decidir si son válidas. Solo en ese caso se ejecuta y se cierra el contrato.
En nuestro caso, este script comprueba que la cantidad que recibirá el destinatario es la acordada por ambas partes.

### Recoger peticiones: endpoints
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
Para recoger las peticiones de los usuarios neceistamos declarar los "endpoints" correspondientes como parte del programa. El conjunto de todos los endpoints se denomina "schema". Lo construiremos usando el tipo Endpoint y el operador ```.\/'``` para combinarlos.
Previamente hemos definido los parámetros necesarios para los endpoints, que son la dirección de la cartera destinataria y el cantidad de Ada a vincular con el contrato.

## Bibliografia
- [Plutus Playground](https://playground.plutus.iohkdev.io/)
- [Documentación de Plutus](https://playground.plutus.iohkdev.io/tutorial/index.html)
- [Video ilustrativo del funcionamiento por Clio.1](https://www.youtube.com/watch?v=yQYXfDG63WI&t=3s)
