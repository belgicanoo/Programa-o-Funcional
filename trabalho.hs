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
