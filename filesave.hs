module FileSave where

import System.IO
import FileRead

{-|
Tarefa T4.1
Grava a estrutura de dados de Torneio AVE num ficheiro CSV.
Formato:
NomeTorneio,NumRondas
(Linhas vazias para formatação)
NomeJogador,FramesGanhos,FramesPerdidos,TotalFrames,AVE

Utiliza a EstruturaTorneioAVE definida em FileRead.hs:
type EstruturaTorneioAVE = (String, Int, [PlayerStatsAVE])
type PlayerStatsAVE = (String, Int, Int, Int, Double)
-}
saveTorneioAVE :: String -> EstruturaTorneioAVE -> IO ()
saveTorneioAVE filename (nome, rondas, stats) = do
    handle <- openFile filename WriteMode
    
    -- 1. Gravar cabeçalho: Nome,Rondas
    hPutStrLn handle $ nome ++ "," ++ show rondas
    
    -- 2. Gravar linhas vazias de separação (simulando o ficheiro de exemplo)
    hPutStrLn handle "" 
    hPutStrLn handle "" 
    
    -- 3. Gravar estatísticas dos jogadores
    mapM_ (hPutStrLn handle . formatPlayerStats) stats
    
    hClose handle
  where
    formatPlayerStats :: PlayerStatsAVE -> String
    formatPlayerStats (nome, fg, fp, total, ave) =
        nome ++ "," ++ show fg ++ "," ++ show fp ++ "," ++ show total ++ "," ++ show ave

{-|
Tarefa T4.1
Grava a estrutura de dados de Resultados AVE num ficheiro CSV.
Formato:
Ronda X
Jogador1,Jogador2,Score1,Score2
...
Ronda Y
...

Utiliza a EstruturaResultadosAVE definida em FileRead.hs:
type EstruturaResultadosAVE = [RoundAVE]
type RoundAVE = [MatchResultAVE]
type MatchResultAVE = (String, String, Int, Int)
-}
saveResultadosTorneioAVE :: String -> EstruturaResultadosAVE -> IO ()
saveResultadosTorneioAVE filename rondas = do
    handle <- openFile filename WriteMode
    
    -- Função auxiliar para formatar e gravar uma ronda
    let formatAndWriteRonda (i, jogos) = do
            hPutStrLn handle $ "Ronda " ++ show i
            mapM_ (hPutStrLn handle . formatMatchResult) jogos
    
    -- Gravar cada ronda (RoundAVE)
    mapM_ formatAndWriteRonda (zip [1..] rondas)
    
    hClose handle
  where
    formatMatchResult :: MatchResultAVE -> String
    formatMatchResult (j1, j2, s1, s2) =
        j1 ++ "," ++ j2 ++ "," ++ show s1 ++ "," ++ show s2

{-|
Tarefa T4.2
Grava a estrutura de dados de Torneio de Eliminatórias num ficheiro CSV.
Formato:
NomeTorneio
(Linhas vazias para formatação)
Equipa1
Equipa2
...

Utiliza a EstruturaTorneioElim definida em FileRead.hs:
type EstruturaTorneioElim = (String, [String])
-}
saveTorneioElim :: String -> EstruturaTorneioElim -> IO ()
saveTorneioElim filename (nome, equipas) = do
    handle <- openFile filename WriteMode
    
    -- 1. Gravar o nome do torneio
    hPutStrLn handle nome
    
    -- 2. Gravar linhas vazias de separação (simulando o ficheiro de exemplo)
    hPutStrLn handle "" 
    hPutStrLn handle "" 
    
    -- 3. Gravar a lista de equipas
    mapM_ (hPutStrLn handle) equipas
    
    hClose handle

{-|
Tarefa T4.2
Grava a estrutura de dados de Resultados de Eliminatórias num ficheiro CSV.
Formato:
Ronda X
J1,EquipaA,EquipaB,VencedorA
J2,EquipaC,EquipaD,VencedorC
...

Utiliza a EstruturaResultadosElim definida em FileRead.hs:
type EstruturaResultadosElim = [RondaElim]
type RondaElim = [JogoElim]
type JogoElim = (String, (String, String), Maybe String)
-}
saveResultadosTorneioElim :: String -> EstruturaResultadosElim -> IO ()
saveResultadosTorneioElim filename rondas = do
    handle <- openFile filename WriteMode
    
    -- Função auxiliar para formatar e gravar uma ronda
    let formatAndWriteRonda (i, jogos) = do
            hPutStrLn handle $ "Ronda " ++ show i
            mapM_ (hPutStrLn handle . formatJogoElim) jogos
    
    -- Gravar cada ronda (RondaElim)
    mapM_ formatAndWriteRonda (zip [1..] rondas)
    
    hClose handle
  where
    formatJogoElim :: JogoElim -> String
    formatJogoElim (idJogo, (eq1, eq2), vencedor) =
        let vencedorStr = case vencedor of
                            Just v -> v
                            -- Deixa em branco se o vencedor for Nothing
                            Nothing -> ""
        in idJogo ++ "," ++ eq1 ++ "," ++ eq2 ++ "," ++ vencedorStr
