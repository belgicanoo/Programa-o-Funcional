module Pairing where
import FileRead
import Data.List (sortBy, delete)
import Data.Ord (comparing, Down(..))
import Data.Maybe (fromMaybe)

getPlayers :: EstruturaTorneioAVE -> [String]
getPlayers (_, _, stats) = [nome | (nome, _, _, _, _) <- stats]

{-|
Obtém o AVE de um jogador específico.
-}
getStats :: String -> EstruturaTorneioAVE -> Double
getStats playerName (_, _, stats) =
    fromMaybe 0.0 (lookupPlayerAVE playerName stats)
  where
    lookupPlayerAVE :: String -> [PlayerStatsAVE] -> Maybe Double
    lookupPlayerAVE _ [] = Nothing
    lookupPlayerAVE p ((nome, _, _, _, ave):t)
        | p == nome = Just ave
        | otherwise = lookupPlayerAVE p t

{-|
Extrai uma lista plana de todos os jogos já realizados.
-}
getPastMatches :: EstruturaResultadosAVE -> [(String, String)]
getPastMatches rondas = map toPlayerPair (concat rondas)
  where
    toPlayerPair :: MatchResultAVE -> (String, String)
    toPlayerPair (j1, j2, _, _) = (j1, j2)

{-|
Verifica se dois jogadores já se defrontaram, em qualquer ordem. 

-}
havePlayed :: String -> String -> [(String, String)] -> Bool
havePlayed j1 j2 pastMatches =
    (j1, j2) `elem` pastMatches || (j2, j1) `elem` pastMatches

{-|
Função principal
Gera uma nova ronda de emparelhamento AVE.
Ordena os jogadores por AVE  e emparelha-os com o vizinho
mais próximo com quem ainda não tenham jogado.
-}
runAVEparing :: EstruturaTorneioAVE -> EstruturaResultadosAVE -> EstruturaResultadosAVE
runAVEparing torneioAVE resultadosAVE =
    let -- Obter jogadores e ordená-los por AVE (descendente) 
        playerNames = getPlayers torneioAVE
        -- Compara usando 'Down' para ordem descendente
        sortedPlayers = sortBy (comparing (Down . (`getStats` torneioAVE))) playerNames
        
        -- Obter histórico de jogos 
        pastMatches = getPastMatches resultadosAVE
        
        -- Gerar novos emparelhamentos (os resultados são 0-0 por defeito)
        newRound = pairPlayers sortedPlayers pastMatches
        
    -- Adiciona a nova ronda à lista de rondas existente 
    in resultadosAVE ++ [newRound]

{-|
Função auxiliar recursiva para emparelhar jogadores para runAVEparing
Tenta emparelhar o primeiro da lista (p1) com o melhor classificado (px)
com quem ainda não tenha jogado
-}
pairPlayers :: [String]         -- Jogadores ordenados por AVE
            -> [(String, String)] -- Jogos passados
            -> RoundAVE           -- Nova ronda (lista de MatchResultAVE)
pairPlayers [] _ = []
pairPlayers [_] _ = [] -- Caso ímpar (não deve acontecer na prática)
pairPlayers (p1:rest) pastMatches =
    -- Encontra um adversário para p1 em 'rest'
    case findOpponent p1 rest pastMatches of
        -- Não encontrou adversário inédito. Emparelha com o primeiro (pode ser repetido)
        -- Isto evita que o programa falhe, embora viole a regra 'sempre que possível' 
        Nothing ->
            let p2 = head rest
                remaining = tail rest
            in (p1, p2, 0, 0) : pairPlayers remaining ( (p1,p2) : pastMatches )
        
        -- Encontrou adversário inédito p2
        Just (p2, remainingPlayers) ->
            -- Adiciona o jogo (p1, p2) à nova ronda
            -- Adiciona (p1, p2) ao 'pastMatches' para a chamada recursiva
            (p1, p2, 0, 0) : pairPlayers remainingPlayers ( (p1,p2) : pastMatches )
  where
    {-|
    Encontra o primeiro adversário 'p' em 'players' tal que (p1, p) não está em 'past'.
    Devolve Maybe (AdversárioEncontrado, ListaRestanteSemAdversário)
    -}
    findOpponent :: String -> [String] -> [(String, String)] -> Maybe (String, [String])
    findOpponent _ [] _ = Nothing -- Ninguém para emparelhar
    findOpponent p1 (p:ps) past
        -- Se p1 e p não jogaram, encontrámos o par
        | not (havePlayed p1 p past) = Just (p, ps)
        -- Se já jogaram, continua a procurar em 'ps'
        | otherwise =
            case findOpponent p1 ps past of
                Nothing -> Nothing -- Não encontrou em 'ps'
                -- Se encontrou 'foundPlayer' em 'ps', temos de adicionar 'p'
                -- de volta à lista restante
                Just (foundPlayer, remaining) -> Just (foundPlayer, p : remaining)

{-|
O objetivo é gerar *todas* as rondas de um torneio de eliminatórias.
Assume que a lista de equipas é potência de 2.
O emparelhamento é determinístico (1º vs 2º, 3º vs 4º...).
O "sorteio"  deve ser feito *antes* de chamar esta função,
baralhando a lista de equipas na EstruturaTorneioElim (Tarefa 5).

-}
runElimParing :: EstruturaTorneioElim -> EstruturaResultadosElim
runElimParing (_, equipas) =
    gerarRondas equipas 1
  where
    gerarRondas :: [String]  -- Lista de participantes (ex: ["SC Braga", "FC Porto", ...])
                -> Int       -- Contador de Jogo (ex: 1 para "J1")
                -> [RondaElim] -- Lista de rondas geradas
    
    -- Caso base: Resta apenas 1 equipa (o vencedor), não há mais rondas para gerar.
    -- CORRETO (e mais idiomático, se a variável não for usada)
    gerarRondas [_] _ = []
    
    -- Caso recursivo: Emparelha as equipas duas a duas.
    gerarRondas participantes gameCounter =
        let -- 1. Cria os jogos para a ronda atual
            (thisRound, nextParticipants, nextGameCounter) = criarRonda participantes gameCounter
            
            -- 2. Gera recursivamente as rondas seguintes
            futureRounds = gerarRondas nextParticipants nextGameCounter
            
        -- 3. Devolve a ronda atual seguida das rondas futuras
        in thisRound : futureRounds

    criarRonda :: [String] -> Int -> (RondaElim, [String], Int)
    criarRonda [] gameCounter = ([], [], gameCounter)
    
    -- Caso recursivo: Pega nos dois primeiros (e1, e2) e emparelha-os
    criarRonda (e1:e2:es) gameCounter =
        let gameId = "J" ++ show gameCounter
            jogo = (gameId, (e1, e2), Nothing)
            
            nextParticipant = "Vencedor " ++ gameId
            
            (jogosRestantes, partsRestantes, nextCounter) = criarRonda es (gameCounter + 1)
            
        -- Agrega os resultados
        in (jogo : jogosRestantes, nextParticipant : partsRestantes, nextCounter)
    
    criarRonda [e] gameCounter = ([], [e], gameCounter)
