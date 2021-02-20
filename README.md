# Smart Contract con Plutus
:office: Universidad de Huelva (UHU)  
:calendar: Curso 2020-2021  
:mortar_board: Modelos Avanzados de Computación  
:octocat: [Ihar Myshkevich (@IgorMy)](https://github.com/IgorMy)  
:octocat: [Víctor M. Rodríguez (@VictorNarov)](https://github.com/VictorNarov)  

## Introducción
En este repositorio se documentara la implementación de un Smart Contract implementado en lenguaje Haskell en la plataforma Plutus Playground. 
En este, se visualiza como una cartera genera un contrato de transferencia de capital (Ada) y como la carpeta destinataria recoge ese capital. 

## Partes del Smart Contract
En este apartado veremos las diferentes partes de este Smart Contract.

### Librerías
```
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
```
En este apotado importamos las librerías de CARDANO para la ejecución del Smart Contract. Su funcionalidad esta comentada a la derecha de su declaración.

## Bibliografia
- [Plutus Playground](https://playground.plutus.iohkdev.io/)
- [Documentación de Plutus](https://playground.plutus.iohkdev.io/tutorial/index.html)
- [Video ilustrativo del funcionamiento por Clio.1](https://www.youtube.com/watch?v=yQYXfDG63WI&t=3s)
