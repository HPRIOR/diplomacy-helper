module Hint exposing (StageCode(..), StageNeeds, getStageStatus)


type StageCode
    = Complete
    | Continue
    | Error


type alias StageNeeds =
    { neededNext : List String
    , currentStatus : StageCode
    }


type alias Evaluator =
    StageNeeds -> StageNeeds


getNextStageNeeds : String -> StageNeeds -> (String -> StageNeeds) -> StageNeeds
getNextStageNeeds input stageNeeds getNextStageNeedsFrom =
    if stageNeeds.neededNext |> List.member input then
        getNextStageNeedsFrom input

    else
        { stageNeeds | currentStatus = Error }


fullEvaluator : (String -> StageNeeds) -> String -> StageNeeds -> StageNeeds
fullEvaluator getNextStageNeedsFromInput input prevStage =
    case ( prevStage.neededNext, prevStage.currentStatus ) of
        ( _, Error ) ->
            -- pass through errors if any have previously occured
            prevStage

        ( needed, code ) ->
            getNextStageNeeds
                input
                { neededNext = needed, currentStatus = code }
                getNextStageNeedsFromInput


stages : List (String -> StageNeeds)
stages =
    [ \_ -> { neededNext = [ "f", "a" ], currentStatus = Continue }
    , \_ -> { neededNext = [ "country" ], currentStatus = Continue }
    , \_ -> { neededNext = [ "move", "->", "supports" ], currentStatus = Continue }
    , \input ->
        case input of
            "supports" ->
                { neededNext = [ "f", "a" ], currentStatus = Continue }

            _ ->
                { neededNext = [ "country" ], currentStatus = Continue }
    , \input ->
        case input of
            "country" ->
                { neededNext = [], currentStatus = Complete }

            _ ->
                { neededNext = [ "country" ], currentStatus = Continue }
    , \input ->
        case input of
            "country" ->
                { neededNext = [ "move", "->" ], currentStatus = Complete }

            _ ->
                { neededNext = [ "move", "->" ], currentStatus = Continue }
    , \_ -> { neededNext = [ "country" ], currentStatus = Continue }
    , \_ -> { neededNext = [], currentStatus = Complete }
    , \_ -> { neededNext = [], currentStatus = Error }
    ]


{-| Partial application returns a series of functions with stages and input fullfilled
-}
getPartialEvaluators : List String -> List Evaluator
getPartialEvaluators input =
    List.map2
        Tuple.pair
        stages
        input
        |> List.map
            (\( stageNeeds, str ) -> fullEvaluator stageNeeds str)


{-| Each partially applied function is given the results of the previous stage in order to 'bubble up' errors

The last result will be returned which can indicating the status and requirements at the given input state (Error, Continue, Complete)

-}
applyEvaluators : List Evaluator -> StageNeeds
applyEvaluators evaluators =
    case evaluators of
        [] ->
            -- seed algorithm with empty base case
            { neededNext = [ "" ]
            , currentStatus = Continue
            }

        first :: rest ->
            first (applyEvaluators rest)


{-| Returns the result of the status of the input state along with any requirements for the next part of the input
-}
getStageStatus : String -> StageNeeds
getStageStatus str =
    ""
        :: String.words str
        |> getPartialEvaluators
        |> List.reverse
        |> applyEvaluators
