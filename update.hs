module Update where
import FileRead
import Pairing

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

recalculateAllAVE :: EstruturaResultadosAVE -> EstruturaTorneioAVE -> EstruturaTorneioAVE
recalculateAllAVE resultados (nome, rondas, stats) =
    let players      = map (\(n,_,_,_,_) -> n) stats
        combMatrix   = buildCombinationsAVE resultados players
        gScores      = buildGameScoresAVE  resultados players
        aveScores    = buildAVEscoresAVE   combMatrix gScores
        newStats = zipWith
                    (\(n, fg, fp, t, _) (_, ave) -> (n, fg, fp, t, ave))
                    stats
                    aveScores
    in (nome, rondas, newStats)

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

updateElim :: String -> String -> EstruturaResultadosElim -> EstruturaResultadosElim
updateElim gameId winnerName results =
    let 
        resultsWithWinner = updateGameResult gameId winnerName results
        resultsFinal = advanceWinnerToNextRounds gameId winnerName resultsWithWinner
    in 
        resultsFinal

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
