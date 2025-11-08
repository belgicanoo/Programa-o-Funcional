import System.IO --Modulo que permete ler e escrever ficheiros
import Text.Printf (printf) --Modulo que permete formatar strings

splitOnComma :: String -> [String] --Função para dividir linhas em colunas com base na vírgula
splitOnComma [] = [""]
splitOnComma (',':xs) = "" : splitOnComma xs
splitOnComma (x:xs) = let (y:ys) = splitOnComma xs in (x:y):ys

readCSV :: FilePath -> IO [[String]] --Função para ler ficheiros CSV e retornar uma lista de listas de strings
readCSV path = do
    contents <- readFile path
    let linhas = lines contents
    return [map trim (splitOnComma linha) | linha <- linhas, not (null linha)]

trim :: String -> String --Função para remover espaços em branco no início e no fim de uma string
trim = f . f
  where f = reverse . dropWhile (`elem` " \t\r\n")

printCSV :: [[String]] -> IO () --Função para imprimir a lista de listas de strings formatada
printCSV = mapM_ (putStrLn . concatMap (printf "%-20s"))


main :: IO () --Função principal que lê e imprime os ficheiros CSV
main = do
    torneioAve <- readCSV "torneio_ave_vila_real.csv"
    resultadosAve <- readCSV "resultados_torneio_ave_vila_real.csv"
    torneioElim <- readCSV "torneio_16_clubes.csv"
    resultadosElim <- readCSV "resultados_torneio_16_clubes.csv"

    putStrLn "=== Torneio AVE ==="
    printCSV torneioAve

    putStrLn "\n=== Resultados Torneio AVE ==="
    printCSV resultadosAve

    putStrLn "\n=== Torneio Eliminação ==="
    printCSV torneioElim

    putStrLn "\n=== Resultados Torneio Eliminação ==="
    printCSV resultadosElim 

    -- >>> ADICIONADO PARA T2.1 - EXEMPLO DE USO
    putStrLn "\n=== Nova Ronda (T2.1 - AVE Pairing) ==="
    let torneioTeste = TorneioAVE "Torneio AVE Vila Real" (map head torneioAve)
        resultadosTeste = ResultadosAVE [] [("A",5,3),("B",4,4),("C",3,5),("D",6,2)]
        novosResultados = runAVEparing torneioTeste resultadosTeste
    print novosResultados



-- >>> ADICIONADO PARA A TAREFA 2.1 <<<

import Data.List (sortBy)
import Data.Ord (comparing)

-- Estruturas de dados simples para representar torneio AVE
type Jogador = String
type Resultado = (Jogador, Int, Int) -- (nome, frames ganhos, frames perdidos)
type Jogo = (Jogador, Jogador)       -- Emparelhamento de dois jogadores
type Ronda = [Jogo]

-- Estrutura do torneio e dos resultados
data TorneioAVE = TorneioAVE {
    nomeTorneio :: String,
    jogadores :: [Jogador]
} deriving (Show, Read)

data ResultadosAVE = ResultadosAVE {
    rondas :: [Ronda],
    historico :: [(Jogador, Int, Int)]  -- (Jogador, total ganhos, total perdidos)
} deriving (Show, Read)


-- | Calcula o AVE de um jogador
calcAVE :: Resultado -> Float
calcAVE (_, ganhos, perdidos)
    | total == 0 = 0
    | otherwise = fromIntegral ganhos / fromIntegral total
  where
    total = ganhos + perdidos


-- | Ordena jogadores por AVE (maior primeiro)
ordenarPorAVE :: [Resultado] -> [Resultado]
ordenarPorAVE = sortBy (flip (comparing calcAVE))


-- | Emparelha jogadores com AVE semelhante
emparelharAVE :: [Resultado] -> [Jogo]
emparelharAVE [] = []
emparelharAVE [_] = []  -- jogador sem adversário
emparelharAVE (x:y:xs) = (nome x, nome y) : emparelharAVE xs
  where
    nome (n,_,_) = n


-- | Verifica se dois jogadores já jogaram entre si
jaJogaram :: Jogo -> [Ronda] -> Bool
jaJogaram (a,b) rondas = any (\r -> (a,b) `elem` r || (b,a) `elem` r) rondas


-- | Cria uma nova ronda AVE com base nos resultados atuais
runAVEparing :: TorneioAVE -> ResultadosAVE -> ResultadosAVE
runAVEparing torneio resultados =
    let listaOrdenada = ordenarPorAVE (historico resultados)
        novaRonda = emparelharComEvitarRepeticao listaOrdenada (rondas resultados)
    in resultados { rondas = rondas resultados ++ [novaRonda] }


-- | Emparelhamento que evita repetições
emparelharComEvitarRepeticao :: [Resultado] -> [Ronda] -> Ronda
emparelharComEvitarRepeticao [] _ = []
emparelharComEvitarRepeticao [x] _ = [] -- jogador sem adversário
emparelharComEvitarRepeticao (x:y:xs) anteriores
    | jaJogaram (n1,n2) anteriores =
        emparelharComEvitarRepeticao (x:xs ++ [y]) anteriores -- tenta outro emparelhamento
    | otherwise = (n1,n2) : emparelharComEvitarRepeticao xs anteriores
  where
    n1 = nome x
    n2 = nome y
    nome (n,_,_) = n
