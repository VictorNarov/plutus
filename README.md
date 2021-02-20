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
import Playground.Contract                                     -- Librería base para contratos en el entorno Plutus Playground.
import           Control.Monad             (void)              -- Funciónes de cálculo avanzados.
import           Data.Aeson                (FromJSON, ToJSON)  -- Tipos y funciones para trabajar eficazmente con datos JSON.
import qualified Data.Text                 as T                -- Tipos y funciones para trabajar eficazmente con texto plano.
import           GHC.Generics              (Generic)           -- Funciones para la conversión de datos.
import           Language.Plutus.Contract                      -- Contratos Pulutus.
import qualified Language.PlutusTx         as PlutusTx         -- Bibliotecas y el compilador para compilar Haskell en Plutus.
import           Language.PlutusTx.Prelude                     -- Un sustituto del Haskell prelude que funciona mejor con PlutusTx.
import           Ledger                                        -- Contenedor para almecenar datos en ruto de una forma mas eficiente.
import qualified Ledger.Ada                as Ada              -- Almacenamiento de tipo ada.
import qualified Ledger.Constraints        as Constraints      -- Restricciones del almacenamiento.
import qualified Ledger.Typed.Scripts      as Scripts          -- Funciones del almacenamiento.
import           Schema                                        -- Bblioteca Haskell para serializar y deserializar datos en JSON.
import           Wallet.Emulator.Wallet                        -- Biblioteca Haskell para gestionar carteras virtuales.
```
En este apotado importamos las librerías de CARDANO para la ejecución del Smart Contract. Su funcionalidad esta comentada a la derecha de su declaración.

## Bibliografia
- [Plutus Playground](https://playground.plutus.iohkdev.io/)
- [Documentación de Plutus](https://playground.plutus.iohkdev.io/tutorial/index.html)
- [Video ilustrativo del funcionamiento por Clio.1](https://www.youtube.com/watch?v=yQYXfDG63WI&t=3s)
