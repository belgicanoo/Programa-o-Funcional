module Update where

import FileRead
import Pairing
import Data.List (mapMaybe)

type GameUpdateElim = (String, String)

------------------------------------------
-- Funções para Torneio AVE (T3.1)
------------------------------------------

updatePlayerRaw :: MatchResultAVE -> PlayerStatsAVE -> PlayerStatsAVE
updatePlayerRaw (j1, j2, s1, s2) (nome, fg, fp, total, ave)
    | nome == j1 =
        let fg'    = fg + s1
            fp'    = fp + s2
            total' = fg' + fp' 
        in (nome, fg', fp', total', ave)
    
    | nome == j2 =
        let fg'    = fg + s2
            fp'    = fp + s1
            total' = fg' + fp' 
        in (nome, fg', fp', total', ave)
        
    | otherwise = (nome, fg, fp, total, ave)

buildCombinationsAVE :: EstruturaResultadosAVE -> [String] -> [[String]]
buildCombinationsAVE resultados players = 
    map (\p -> concatMap (findOpponent p) resultados) players
  where
    findOpponent :: String -> RoundAVE -> [String]
    findOpponent p round = mapMaybe (getOpponent p) round
    
    getOpponent :: String -> MatchResultAVE -> Maybe String
    getOpponent p (j1, j2, _, _)
      | p == j1 = Just j2
      | p == j2 = Just j1
      | otherwise = Nothing

buildGameScoresAVE :: EstruturaResultadosAVE -> [String] -> [[(String, Double)]]
buildGameScoresAVE _ players = map (\p -> [(p, 0.0)]) players 

buildAVEscoresAVE :: [PlayerStatsAVE] -> [[String]] -> [(String, Double)]
buildAVEscoresAVE stats combMatrix = 
    zip (map (\(n,_,_,_,_) -> n) stats) (map calculateAVE (zip stats combMatrix))
  where
    getOpponentAVE :: String -> Double
    getOpponentAVE pName = case filter (\(n,_,_,_,_) -> n == pName) stats of
                                ((_, _, _, _, ave):_) -> ave
                                []                    -> 0.0
    
    calculateOAVE :: [String] -> Double
    calculateOAVE [] = 1.0
    calculateOAVE opponents = 
        let opponentAVEs = map getOpponentAVE opponents
        in sum opponentAVEs / fromIntegral (length opponentAVEs)
    
    calculateAVE :: (PlayerStatsAVE, [String]) -> Double
    calculateAVE ((_, fg, fp, total, _), opponents)
        | total == 0 = 0.0
        | otherwise = fromIntegral fg / fromIntegral total

recalculateAllAVE :: EstruturaResultadosAVE -> EstruturaTorneioAVE -> EstruturaTorneioAVE
recalculateAllAVE _ (nome, rondas, stats) =
    let 
        newStats = map updateSingleStatAVE stats
    in (nome, rondas, newStats)
  where
    updateSingleStatAVE :: PlayerStatsAVE -> PlayerStatsAVE
    updateSingleStatAVE (nome, fg, fp, total, _)
        | total == 0 = (nome, fg, fp, total, 0.0)
        | otherwise  = (nome, fg, fp, total, fromIntegral fg / fromIntegral total)

updateTournamentStats :: MatchResultAVE -> EstruturaResultadosAVE -> EstruturaTorneioAVE -> EstruturaTorneioAVE
updateTournamentStats jogo resultados torneio =
    let torneioFGFP = updateFGFP jogo torneio
    in recalculateAllAVE resultados torneioFGFP

updateFGFP :: MatchResultAVE -> EstruturaTorneioAVE -> EstruturaTorneioAVE
updateFGFP jogo (nome, rondas, stats) =
    let stats' = map (updatePlayerRaw jogo) stats
    in (nome, rondas, stats')

addGameToLastRound :: MatchResultAVE -> EstruturaResultadosAVE -> EstruturaResultadosAVE
addGameToLastRound jogo [] = [[jogo]]    
addGameToLastRound jogo rs =
    let initRondas = init rs
        lastRonda  = last rs
    in initRondas ++ [lastRonda ++ [jogo]]

updateAVE :: MatchResultAVE 
          -> EstruturaTorneioAVE
          -> EstruturaResultadosAVE
          -> (EstruturaTorneioAVE, EstruturaResultadosAVE)
updateAVE jogo torneio resultados =
    let resultados' = addGameToLastRound jogo resultados
        torneio'    = updateTournamentStats jogo resultados' torneio
    in (torneio', resultados')


------------------------------------------
-- Funções para Torneio Eliminatórias (T3.2)
------------------------------------------

updateElim :: String 
           -> String 
           -> EstruturaTorneioElim
           -> EstruturaResultadosElim 
           -> (EstruturaTorneioElim, EstruturaResultadosElim)
updateElim gameId winnerName torneio results =
    let 
        resultsWithWinner = updateGameResult gameId winnerName results
        resultsFinal = advanceWinnerToNextRounds gameId winnerName resultsWithWinner
    in 
        (torneio, resultsFinal)


updateGameResult :: String -> String -> EstruturaResultadosElim -> EstruturaResultadosElim
updateGameResult gameId winnerName = map (map (updateSingleGame gameId winnerName))
    where
        updateSingleGame :: String -> String -> JogoElim -> JogoElim
        updateSingleGame targetId newWinner (id, players, Nothing)
            | id == targetId = (id, players, Just newWinner)
        updateSingleGame _ _ jogo = jogo

advanceWinnerToNextRounds :: String -> String -> EstruturaResultadosElim -> EstruturaResultadosElim
advanceWinnerToNextRounds gameId winnerName rounds = 
    let 
        placeholder = "Vencedor " ++ gameId
    in map (map (updateGamePlayers placeholder winnerName)) rounds

    where
        updateGamePlayers :: String -> String -> JogoElim -> JogoElim
        updateGamePlayers placeholderName newWinner (id, (p1, p2), winner) =
            let 
                p1' = if p1 == placeholderName then newWinner else p1
                p2' = if p2 == placeholderName then newWinner else p2
            in (id, (p1', p2'), winner)
