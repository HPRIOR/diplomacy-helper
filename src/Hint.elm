module Hint exposing (StageCode(..), StageNeeds, getStageStatus)


type StageCode
    = Complete
    | Continue
    | Error


type alias StageNeeds =
    { neededNext : List String
    , stageCode : StageCode
    }


type alias Evaluator =
    StageNeeds -> StageNeeds


getNewStageStatus : String -> StageNeeds -> (String -> StageNeeds) -> StageNeeds
getNewStageStatus input stageNeeds nextStageNeeds =
    if stageNeeds.neededNext |> List.member input then
        nextStageNeeds input

    else
        { stageNeeds | stageCode = Error }


fullEvaluator : (String -> StageNeeds) -> String -> StageNeeds -> StageNeeds
fullEvaluator nextStageNeeds input prevStage =
    case ( prevStage.neededNext, prevStage.stageCode ) of
        ( _, Error ) ->
            prevStage

        ( needed, code ) ->
            getNewStageStatus
                input
                { neededNext = needed, stageCode = code }
                nextStageNeeds


stages : List (String -> StageNeeds)
stages =
    [ \_ -> { neededNext = [ "f", "a" ], stageCode = Continue }
    , \_ -> { neededNext = [ "country" ], stageCode = Continue }
    , \_ -> { neededNext = [ "move", "->", "supports" ], stageCode = Continue }
    , \input ->
        case input of
            "supports" ->
                { neededNext = [ "f", "a" ], stageCode = Continue }

            _ ->
                { neededNext = [ "country" ], stageCode = Continue }
    , \input ->
        case input of
            "country" ->
                { neededNext = [], stageCode = Complete }

            _ ->
                { neededNext = [ "country" ], stageCode = Continue }
    , \input ->
        case input of
            "country" ->
                { neededNext = [ "move", "->" ], stageCode = Complete }

            _ ->
                { neededNext = [ "move", "->" ], stageCode = Continue }
    , \_ -> { neededNext = [ "country" ], stageCode = Continue }
    , \_ -> { neededNext = [], stageCode = Complete }
    , \_ -> { neededNext = [], stageCode = Error }
    ]


getPartialEvaluators : List String -> List Evaluator
getPartialEvaluators input =
    List.map2
        Tuple.pair
        stages
        input
        |> List.map
            (\( stageNeeds, str ) -> fullEvaluator stageNeeds str)


applyEvaluators : List Evaluator -> StageNeeds
applyEvaluators evaluators =
    case evaluators of
        [] ->
            -- seed algorithm with empty base case
            { neededNext = [ "" ]
            , stageCode = Continue
            }

        first :: rest ->
            first (applyEvaluators rest)


getStageStatus : String -> StageNeeds
getStageStatus str =
    ""
        :: String.words str
        |> getPartialEvaluators
        |> List.reverse
        |> applyEvaluators
