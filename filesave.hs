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
