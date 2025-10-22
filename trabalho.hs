module Trabalho where
import System.IO

------- Dados

type JogadorAVE = (String, Int, Int, Float)
type TorneioAVE = [JogadorAVE]
type ResultadoAVE = (Int, String, String, String)
type ResultadosAVE = [ResultadoAVE]
type Equipa = String
type TorneioElim = [Equipa]
type ResultadoElim = (Int, String, String, String)
type ResultadosElim = [ResultadoElim]
