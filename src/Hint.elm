module Hint exposing (StageCategory(..), StageCode(..), StageNeeds, getStageStatus)


countries : List String
countries =
    [ "ADR", "AEG", "Alb", "Ank", "Apu", "Arm", "BAL", "BAR", "Bel", "Ber", "BLA", "Boh", "Bre", "Bud", "Bul", "Bur", "Cly", "Con", "Den", "EMS", "Edi", "ENG", "Fin", "Gal", "Gas", "GOB", "GOL", "Gre", "HEL", "Hol", "ION", "IRS", "Kie", "Liv", "Lvp", "Lon", "MAO", "Mar", "Mos", "Mun", "Naf", "NAO", "Nap", "NTH", "Nwy", "NWG", "Par", "Pic", "Pie", "Por", "Pru", "Rom", "Ruh", "Rum", "Ser", "Sev", "Sil", "SKA", "Smy", "Spa", "Stp", "Swe", "Syr", "Tri", "Tun", "Tus", "Tyr", "TYS", "Ukr", "Ven", "Vie", "Wal", "War", "WMS", "Yor" ]


type StageError
    = InputError


type StageCategory
    = UnitType
    | Country
    | Command
    | None


type StageCode
    = Complete
    | Continue
    | Error StageError


type alias StageNeeds =
    { neededNext : List String
    , stageCategory : StageCategory
    , currentStatus : StageCode
    }


type alias Evaluator =
    StageNeeds -> StageNeeds


getNextStageNeeds : String -> StageNeeds -> (String -> StageNeeds) -> StageNeeds
getNextStageNeeds input stageNeeds getNextStageNeedsFrom =
    if stageNeeds.neededNext |> List.member input then
        getNextStageNeedsFrom input

    else
        -- no requirements have been fullfulled by current input
        { stageNeeds | currentStatus = Error InputError }


fullEvaluator : (String -> StageNeeds) -> String -> StageNeeds -> StageNeeds
fullEvaluator getNextStageNeedsFromInput input prevStage =
    case ( prevStage.neededNext, prevStage.currentStatus, prevStage.stageCategory ) of
        ( _, Error _, _ ) ->
            -- pass through errors if any have previously occured
            prevStage

        ( needed, code, category ) ->
            getNextStageNeeds
                input
                { neededNext = needed, currentStatus = code, stageCategory = category }
                getNextStageNeedsFromInput


stages : List (String -> StageNeeds)
stages =
    [ \_ -> { neededNext = [ "f", "a" ], currentStatus = Continue, stageCategory = UnitType }
    , \_ -> { neededNext = countries, currentStatus = Continue, stageCategory = Country }
    , \_ -> { neededNext = [ "move", "->", "supports" ], currentStatus = Continue, stageCategory = Command }
    , \input ->
        case input of
            "supports" ->
                { neededNext = [ "f", "a" ], currentStatus = Continue, stageCategory = UnitType }

            _ ->
                { neededNext = countries, currentStatus = Continue, stageCategory = Country }
    , \input ->
        case input of
            "f" ->
                { neededNext = countries, currentStatus = Continue, stageCategory = Country }

            "a" ->
                { neededNext = countries, currentStatus = Continue, stageCategory = Country }

            _ ->
                { neededNext = [], currentStatus = Complete, stageCategory = None }
    , \input ->
        case input of
            "supports" ->
                { neededNext = [ "move", "->" ], currentStatus = Continue, stageCategory = Command }

            _ ->
                { neededNext = [ "move", "->" ], currentStatus = Complete, stageCategory = Country }
    , \_ -> { neededNext = countries, currentStatus = Continue, stageCategory = Country }
    , \_ -> { neededNext = [], currentStatus = Complete, stageCategory = None }
    , \_ -> { neededNext = [], currentStatus = Error InputError, stageCategory = None }
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
            , stageCategory = None
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
