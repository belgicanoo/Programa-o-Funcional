module Application where

import System.IO
import FileRead
import Pairing
import Update
import FileSave
import Data.List (sortBy) 

origAveTorneio   = "torneio_ave_vila_real.csv"
origAveResult    = "resultados_torneio_ave_vila_real.csv"
origElimTorneio  = "torneio_16_clubes.csv"
origElimResult   = "resultados_torneio_16_clubes.csv"
saveAveTorneio   = "output_ave_torneio.csv"
saveAveResult    = "output_ave_resultados.csv"
saveElimTorneio  = "output_elim_torneio.csv"
saveElimResult   = "output_elim_resultados.csv"



main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putStrLn "\n=== MENU INICIAL ==="
    putStrLn "1: Novo torneio AVE"
    putStrLn "2: Novo torneio de eliminações diretas"
    putStrLn "3: Continuar torneio AVE salvado"
    putStrLn "4: Continuar torneio de elimnações diretas salvado"
    putStrLn "0. Sair\n"
    
    putStr "Escolha sua opção: "
    option <- getLine

    case option of
        "1" -> runTesteAVE origAveTorneio origAveResult
        "2" -> runTesteElim origElimTorneio origElimResult
        "3" -> runTesteAVE saveAveTorneio saveAveResult
        "4" -> runTesteElim saveElimTorneio saveElimResult
        "0" -> putStrLn "A sair..."
        _   -> do 
            putStrLn "Opção inválida. Tente novamente."
            main

runTesteAVE :: String -> String -> IO ()
runTesteAVE fTorneio fResultados = do
    putStrLn "\n--- MENU AVE ---"
    putStrLn "1: Gerar nova ronda"
    putStrLn "2: Inserir resultado do jogo"
    putStrLn "3: Ver pódio"
    putStrLn "0: Voltar ao menu inicial\n"
    
    putStr "Opção escolhida: "
    op <- getLine
    
    case op of
        "1" -> do

            torneio <- readTorneioAVE fTorneio
            resultados <- readResultadosTorneioAVE fResultados

            printTorneioAVE torneio
            putStrLn "\n[Processamento] A gerar emparelhamento..."
            let resultadosComNovaRonda = runAVEparing torneio resultados
            printResultadosTorneioAVE resultadosComNovaRonda
            
            putStrLn "[Auto-Save] A gravar nova ronda..."
            saveResultadosTorneioAVE saveAveResult resultadosComNovaRonda
            
            runTesteAVE saveAveTorneio saveAveResult

        "2" -> do
            torneio <- readTorneioAVE fTorneio
            resultados <- readResultadosTorneioAVE fResultados
            
            putStrLn "\nInserir resultado:"
            putStr "Nome Jogador 1: "; j1 <- getLine
            putStr "Nome Jogador 2: "; j2 <- getLine
            putStr "Pontos J1: "; s1 <- readLn
            putStr "Pontos J2: "; s2 <- readLn
            
            let (torneioAtualizado, resultadosAtualizados) = updateAVE (j1, j2, s1, s2) torneio resultados
          
            putStrLn "A gravar alterações..."
            saveTorneioAVE saveAveTorneio torneioAtualizado
            saveResultadosTorneioAVE saveAveResult resultadosAtualizados
            
            putStrLn "\n--- Quadro Atualizado ---"
            printResultadosTorneioAVE resultadosAtualizados
            --Recursividade
            runTesteAVE saveAveTorneio saveAveResult

        "3" -> do
            (nomeT, nRondas, stats) <- readTorneioAVE fTorneio
            let sortedStats = sortBy (\(_,_,_,_,a1) (_,_,_,_,a2) -> compare a2 a1) stats
            
            putStrLn "\n=== PÓDIO ATUAL ==="
            printTorneioAVE (nomeT, nRondas, sortedStats)
            
            runTesteAVE fTorneio fResultados

        "0" -> main
        _   -> runTesteAVE fTorneio fResultados


runTesteElim :: String -> String -> IO ()
runTesteElim fTorneio fResultados = do
    putStrLn $ "\n[A Ler] " ++ fTorneio ++ "..."

    torneio <- readTorneioElim fTorneio
    resultados <- readResultadosTorneioElim fResultados

    let grelhaTrabalho = if null resultados 
                         then runElimParing torneio 
                         else resultados

    putStrLn "\n[Estado da Grelha]"
    printResultadosTorneioElim grelhaTrabalho

    putStrLn "\nOpções:"
    putStrLn "1. Inserir Vencedor"
    putStrLn "0. Voltar"
    op <- getLine

    case op of
        "1" -> do
            putStrLn "\nDefina um vencedor:"
            putStr "ID do Jogo (ex: J1): "
            gid <- getLine
            putStr "Nome do Vencedor: "
            vencedor <- getLine

            let (torneioUpd, grelhaUpd) = updateElim (gid, vencedor) torneio grelhaTrabalho
            
            putStrLn "\nA gravar..."
            saveTorneioElim saveElimTorneio torneioUpd
            saveResultadosTorneioElim saveElimResult grelhaUpd
            
            runTesteElim saveElimTorneio saveElimResult

        "0" -> main
        _ -> runTesteElim fTorneio fResultados
