module FileRead where

import System.IO
import Text.Read (readMaybe)
import Data.List (findIndex)
import Data.Maybe (mapMaybe)

type PlayerStatsAVE = (String, Int, Int, Int, Double)

type EstruturaTorneioAVE = (String, Int, [PlayerStatsAVE])

type MatchResultAVE = (String, String, Int, Int)

type RoundAVE = [MatchResultAVE]

type EstruturaResultadosAVE = [RoundAVE]

type EstruturaTorneioElim = (String, [String])

type JogoElim = (String, (String, String), Maybe String)

type RondaElim = [JogoElim]

type EstruturaResultadosElim = [RondaElim]


splitCsvLine :: String -> [String]
splitCsvLine s = case findIndex (==',') s of
    Nothing -> [s]
    Just i  -> take i s : splitCsvLine (drop (i+1) s)

readDef :: Read a => a -> String -> a
readDef def s = fromMaybe def (readMaybe s)
  where
    fromMaybe :: a -> Maybe a -> a
    fromMaybe d Nothing  = d
    fromMaybe _ (Just v) = v

groupByRonda :: [String] -> [String] -> [[String]]
groupByRonda [] [] = []
groupByRonda [] acc = [reverse acc]
groupByRonda (h:t) acc
    | (take 5 h == "Ronda") || null h =
        if null acc
        then groupByRonda t [] 
        else (reverse acc) : groupByRonda t []
    | otherwise = groupByRonda t (h:acc)




readTorneioAVE :: String -> IO EstruturaTorneioAVE
readTorneioAVE filename = do
    content <- readFile filename
    return (parseTorneioAVE content)

parseTorneioAVE :: String -> EstruturaTorneioAVE
parseTorneioAVE content =
    let lns = lines content
        [nome, rondasStr] = splitCsvLine (head lns)
        numRondas = readDef 0 rondasStr
        statsLines = drop 3 lns
        stats = mapMaybe parsePlayerStats statsLines
        
    in (nome, numRondas, stats)
parsePlayerStats :: String -> Maybe PlayerStatsAVE
parsePlayerStats line =
    case splitCsvLine line of
        [nome, fgStr, fpStr, totalStr, aveStr] ->
            Just (nome, readDef 0 fgStr, readDef 0 fpStr, readDef 0 totalStr, readDef 0.0 aveStr)
        _ -> Nothing 

readResultadosTorneioAVE :: String -> IO EstruturaResultadosAVE
readResultadosTorneioAVE filename = do
    content <- readFile filename
    return (parseResultadosAVE content)

parseResultadosAVE :: String -> EstruturaResultadosAVE
parseResultadosAVE content =
    let lns = lines content
        groups = groupByRonda lns []
    in map (mapMaybe parseMatchResultAVE) groups
parseMatchResultAVE :: String -> Maybe MatchResultAVE
parseMatchResultAVE line =
    case splitCsvLine line of
        [j1, j2, s1Str, s2Str] ->
            Just (j1, j2, readDef 0 s1Str, readDef 0 s2Str)
        _ -> Nothing

readTorneioElim :: String -> IO EstruturaTorneioElim
readTorneioElim filename = do
    content <- readFile filename
    return (parseTorneioElim content)

parseTorneioElim :: String -> EstruturaTorneioElim
parseTorneioElim content =
    let lns = lines content
        nome = head lns
        equipas = drop 3 lns
    in (nome, equipas)

readResultadosTorneioElim :: String -> IO EstruturaResultadosElim
readResultadosTorneioElim filename = do
    content <- readFile filename
    return (parseResultadosElim content)

parseResultadosElim :: String -> EstruturaResultadosElim
parseResultadosElim content =
    let lns = lines content
        groups = groupByRonda lns []
    in map (mapMaybe parseJogoElim) groups

parseJogoElim :: String -> Maybe JogoElim
parseJogoElim line =
    case splitCsvLine line of
        [idJogo, eq1, eq2, vencedor] ->
            let mv = if null vencedor then Nothing else Just vencedor
            in Just (idJogo, (eq1, eq2), mv)
        [idJogo, eq1, eq2] ->
            Just (idJogo, (eq1, eq2), Nothing)
        _ -> Nothing

padL :: Int -> String -> String
padL n s
    | len < n   = replicate (n - len) ' ' ++ s
    | otherwise = s
  where len = length s

padR :: Int -> String -> String
padR n s
    | len < n   = s ++ replicate (n - len) ' '
    | otherwise = s
  where len = length s

printTorneioAVE :: EstruturaTorneioAVE -> IO ()
printTorneioAVE (nome, rondas, stats) = do
    putStrLn $ "--- Torneio AVE: " ++ nome ++ " (" ++ show rondas ++ " Rondas) ---"
    putStrLn ""
    putStrLn "Classificação:"
    putStrLn $ padR 4 "Pos" ++ "| " ++ padR 14 "Jogador" ++ "| " ++ padR 9 "Frames G" ++ "| " ++ padR 9 "Frames P" ++ "| " ++ padR 6 "Total" ++ "| AVE"
    putStrLn $ replicate 62 '-'
    mapM_ (putStrLn . formatStat) (zip [1..] stats)
  where
    formatStat :: (Int, PlayerStatsAVE) -> String
    formatStat (pos, (nome, fg, fp, total, ave)) =
        padR 4 (show pos) ++ "| " ++
        padR 14 nome ++ "| " ++
        padR 9 (show fg) ++ "| " ++
        padR 9 (show fp) ++ "| " ++
        padR 6 (show total) ++ "| " ++
        show ave 

printResultadosTorneioAVE :: EstruturaResultadosAVE -> IO ()
printResultadosTorneioAVE rondas = do
    putStrLn "--- Resultados dos Jogos (AVE) ---"
    mapM_ printRonda (zip [1..] rondas)
  where
    printRonda :: (Int, RoundAVE) -> IO ()
    printRonda (i, jogos) = do
        putStrLn $ "\nRonda " ++ show i
        putStrLn $ replicate 10 '-'
        mapM_ (putStrLn . formatJogo) jogos
    
    formatJogo :: MatchResultAVE -> String
    formatJogo (j1, j2, s1, s2) =
        "  " ++ padL 12 j1 ++ " " ++ show s1 ++ " vs " ++ show s2 ++ " " ++ padR 12 j2

printTorneioElim :: String -> EstruturaTorneioElim -> IO ()
printTorneioElim _ (nome, equipas) = do
    putStrLn $ "--- Torneio Eliminatórias: " ++ nome ++ " ---"
    putStrLn ""
    putStrLn $ "Equipas Participantes (" ++ show (length equipas) ++ "):"
    putStrLn $ replicate 30 '-'
    mapM_ (\e -> putStrLn $ "  - " ++ e) equipas

printResultadosTorneioElim :: EstruturaResultadosElim -> IO ()
printResultadosTorneioElim rondas = do
    putStrLn "--- Grelha de Resultados (Eliminatórias) ---"
    mapM_ printRonda (zip [1..] rondasComNome)
  where
    totalRondas = length rondas
    nomesRondas = map (getRondaNome . (totalRondas -)) [0..totalRondas-1]
    rondasComNome = zip nomesRondas rondas

    printRonda :: (Int, (String, RondaElim)) -> IO ()
    printRonda (i, (nomeRonda, jogos)) = do
        putStrLn $ "\nRonda " ++ show i ++ " (" ++ nomeRonda ++ ")"
        putStrLn $ replicate 20 '-'
        mapM_ (putStrLn . formatJogo) jogos
        
    getRondaNome :: Int -> String
    getRondaNome 0 = "Final"
    getRondaNome 1 = "Meias-Finais"
    getRondaNome 2 = "Quartos-de-Final"
    getRondaNome 3 = "Oitavos-de-Final"
    getRondaNome n = show (2 ^ (n+1)) ++ "-avos-de-Final" 

    formatJogo :: JogoElim -> String
    formatJogo (idJogo, (eq1, eq2), vencedor) =
        "  " ++ padR 4 idJogo ++ ": " ++
        padL 15 eq1 ++ " vs " ++ padR 15 eq2 ++
        "  ==> " ++ maybe " (A definir)" id vencedor 
