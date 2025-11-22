module Update where
import FileRead
import Pairing

updatePlayerRaw :: MatchResultAVE -> PlayerStatsAVE -> PlayerStatsAVE
updatePlayerRaw (j1, j2, s1, s2) (nome, fg, fp, total, ave)
    | nome == j1 =
        let fg'    = fg + s1
            fp'    = fp + s2
            total' = fg' - fp'
        in (nome, fg', fp', total', ave)

    | nome == j2 =
        let fg'    = fg + s2
            fp'    = fp + s1
            total' = fg' - fp'
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
