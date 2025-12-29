module FileRead where

import Data.Char (isDigit)
import Data.Function (on)
import Data.List (findIndex, groupBy)
import Data.Maybe (fromMaybe, mapMaybe)
import System.IO
import Text.Read (readMaybe)

-- Estruturas de Dados (Mantêm-se iguais)
type PlayerStatsAVE = (String, Int, Int, Int, Double)
type EstruturaTorneioAVE = (String, Int, [PlayerStatsAVE])
type MatchResultAVE = (String, String, Int, Int)
type RoundAVE = [MatchResultAVE]
type EstruturaResultadosAVE = [RoundAVE]

type EstruturaTorneioElim = (String, [String])
type JogoElim = (String, (String, String), Maybe String)
type RondaElim = [JogoElim]
type EstruturaResultadosElim = [RondaElim]

-- Funções Auxiliares de Parsing (Mantêm-se iguais)
splitCsvLine :: String -> [String]
splitCsvLine s = case findIndex (== ',') s of
  Nothing -> [s]
  Just i -> take i s : splitCsvLine (drop (i + 1) s)

readDef :: (Read a) => a -> String -> a
readDef def s = fromMaybe def (readMaybe s)

parseScore :: String -> (Int, Int)
parseScore s =
  let repl c = if isDigit c then c else ' '
      s' = map repl s
      ws = words s'
   in case ws of
        [a, b] -> (readDef 0 a, readDef 0 b)
        _ -> (0, 0)

padL :: Int -> String -> String
padL n s | length s < n = replicate (n - length s) ' ' ++ s | otherwise = s

padR :: Int -> String -> String
padR n s | length s < n = s ++ replicate (n - length s) ' ' | otherwise = s

-- ========================================================
-- AQUI ESTÃO AS CORREÇÕES IMPORTANTES (USAR readStrict)
-- ========================================================

readTorneioAVE :: String -> IO EstruturaTorneioAVE
readTorneioAVE filename = do
  content <- readStrict filename  -- <--- CORRIGIDO: Usa readStrict em vez de readFile
  return (parseTorneioAVE content)

parseTorneioAVE :: String -> EstruturaTorneioAVE
parseTorneioAVE content =
  let lns = lines content
      safeHead [] = ""
      safeHead (x : _) = x
      line1 = safeHead lns
      parts1 = splitCsvLine line1
      nome = if length parts1 >= 2 then parts1 !! 1 else "Unknown"
      
      line2 = if length lns > 1 then lns !! 1 else ""
      parts2 = splitCsvLine line2
      rondasStr = if length parts2 >= 2 then parts2 !! 1 else "0"
      numRondas = readDef 0 rondasStr

      stats = mapMaybe parsePlayerStats lns
   in (nome, numRondas, stats)

parsePlayerStats :: String -> Maybe PlayerStatsAVE
parsePlayerStats line =
  case splitCsvLine line of
    (nome : _ : fgStr : fpStr : aveStr : _) ->
      if not (null fgStr) && all isDigit fgStr then
          let fg = readDef 0 fgStr
              fp = readDef 0 fpStr
              total = fg + fp
              ave = readDef 0.0 aveStr
           in Just (nome, fg, fp, total, ave)
      else Nothing
    _ -> Nothing

readResultadosTorneioAVE :: String -> IO EstruturaResultadosAVE
readResultadosTorneioAVE filename = do
  content <- readStrict filename -- <--- CORRIGIDO: Usa readStrict
  return (parseResultadosAVE content)

parseResultadosAVE :: String -> EstruturaResultadosAVE
parseResultadosAVE content =
  let lns = lines content
      validLines = filter (\l -> not (null l) && isDigit (head l)) lns
      parsed = mapMaybe parseLineResultAVE validLines
      grouped = groupBy ((==) `on` fst) parsed
   in map (map snd) grouped

parseLineResultAVE :: String -> Maybe (Int, MatchResultAVE)
parseLineResultAVE line =
  case splitCsvLine line of
    (rondaStr : j1 : j2 : resStr : _) ->
      let r = readDef 0 rondaStr
          (s1, s2) = parseScore resStr
       in Just (r, (j1, j2, s1, s2))
    _ -> Nothing

readTorneioElim :: String -> IO EstruturaTorneioElim
readTorneioElim filename = do
  content <- readStrict filename -- <--- CORRIGIDO: Usa readStrict
  return (parseTorneioElim content)

parseTorneioElim :: String -> EstruturaTorneioElim
parseTorneioElim content =
  let lns = lines content
      line1 = if null lns then "" else head lns
      parts1 = splitCsvLine line1
      nome = if length parts1 >= 2 then parts1 !! 1 else "Unknown"
      equipas = mapMaybe parseEquipaElim lns
   in (nome, equipas)

parseEquipaElim :: String -> Maybe String
parseEquipaElim line =
  case splitCsvLine line of
    (numStr : nome : _) ->
        if not (null numStr) && all isDigit numStr 
        then Just nome
        else Nothing
    _ -> Nothing

readResultadosTorneioElim :: String -> IO EstruturaResultadosElim
readResultadosTorneioElim filename = do
  content <- readStrict filename -- <--- CORRIGIDO: Usa readStrict
  return (parseResultadosElim content)

parseResultadosElim :: String -> EstruturaResultadosElim
parseResultadosElim content =
  let lns = lines content
      validLines = filter (\l -> not (null l) && take 5 l /= "Ronda") lns
      parsed = mapMaybe parseLineElim validLines
      grouped = groupBy ((==) `on` fst) parsed
   in map (map snd) grouped

parseLineElim :: String -> Maybe (String, JogoElim)
parseLineElim line =
  let cols = splitCsvLine line
   in if length cols >= 12
        then
          let ronda = cols !! 0
              idJogo = cols !! 1
              eq1 = cols !! 2
              eq2 = cols !! 3
              vencedor = cols !! 11
              mv = if null vencedor || vencedor == "###" then Nothing else Just vencedor
           in Just (ronda, (idJogo, (eq1, eq2), mv))
        else Nothing

-- Funções de Print (Mantêm-se iguais)
printTorneioAVE :: EstruturaTorneioAVE -> IO ()
printTorneioAVE (nome, rondas, stats) = do
  putStrLn $ "\n--- Torneio AVE: " ++ nome ++ " (" ++ show rondas ++ " Rondas) ---"
  putStrLn $ padR 4 "Pos" ++ "| " ++ padR 18 "Jogador" ++ "| " ++ padR 9 "Fr Ganhos" ++ "| " ++ padR 9 "Fr Perd" ++ "| " ++ padR 6 "Total" ++ "| AVE"
  putStrLn $ replicate 66 '-'
  mapM_ (putStrLn . formatStat) (zip [1 ..] stats)
  where
    formatStat (pos, (nome, fg, fp, total, ave)) =
      padR 4 (show pos) ++ "| " ++ padR 18 nome ++ "| " ++ padR 9 (show fg) ++ "| " ++ padR 9 (show fp) ++ "| " ++ padR 6 (show total) ++ "| " ++ show ave

printResultadosTorneioAVE :: EstruturaResultadosAVE -> IO ()
printResultadosTorneioAVE rondas = do
  putStrLn "\n--- Resultados dos Jogos (AVE) ---"
  mapM_ printRonda (zip [1 ..] rondas)
  where
    printRonda (i, jogos) = do
      putStrLn $ "\nRonda " ++ show i
      putStrLn $ replicate 20 '-'
      mapM_ (putStrLn . formatJogo) jogos
    formatJogo (j1, j2, s1, s2) =
      "  " ++ padL 15 j1 ++ " " ++ show s1 ++ " vs " ++ show s2 ++ " " ++ padR 15 j2

printTorneioElim :: EstruturaTorneioElim -> IO ()
printTorneioElim (nome, equipas) = do
  putStrLn $ "\n--- Torneio Eliminatórias: " ++ nome ++ " ---"
  putStrLn $ "Equipas Participantes (" ++ show (length equipas) ++ "):"
  putStrLn $ replicate 30 '-'
  mapM_ (\e -> putStrLn $ "  - " ++ e) equipas

printResultadosTorneioElim :: EstruturaResultadosElim -> IO ()
printResultadosTorneioElim rondas = do
  putStrLn "\n--- Grelha de Resultados (Eliminatórias) ---"
  mapM_ printRonda (zip [1 ..] rondasComNome)
  where
    totalRondas = length rondas
    nomesRondas = map (getRondaNome . (totalRondas -)) [1 .. totalRondas]
    rondasComNome = zip nomesRondas rondas

    printRonda (i, (nomeRonda, jogos)) = do
      putStrLn $ "\nRonda " ++ show i ++ " (" ++ nomeRonda ++ ")"
      putStrLn $ replicate 30 '-'
      mapM_ (putStrLn . formatJogo) jogos

    getRondaNome 0 = "Final"
    getRondaNome 1 = "Meias-Finais"
    getRondaNome 2 = "Quartos-de-Final"
    getRondaNome 3 = "Oitavos-de-Final"
    getRondaNome n = show (2 ^ (n + 1)) ++ "-avos-de-Final"

    formatJogo (idJogo, (eq1, eq2), vencedor) =
      "  " ++ padR 4 idJogo ++ ": " ++ padL 15 eq1 ++ " vs " ++ padR 15 eq2 ++ "  ==> " ++ maybe " (A definir)" id vencedor


--adicional (IA)
readStrict :: FilePath -> IO String
readStrict ficheiro = do
    h <- openFile ficheiro ReadMode
    conteudo <- hGetContents h
    length conteudo `seq` return ()  
    hClose h                         
    return conteudo
