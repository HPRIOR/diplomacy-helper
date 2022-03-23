module Hint exposing (StageCode(..), StageNeeds, getStageStatus, StageCategory(..))

countries : List String
countries =
    [ "Adriatic Sea", "adr", "adriatic", "Aegean Sea", "aeg", "aegean", "Albania", "alb", "Ankara", "ank", "Apulia", "apu", "Armenia", "arm", "Baltic Sea", "bal", "baltic", "Barents Sea", "bar", "barents", "Belgium", "bel", "Berlin", "ber", "Black Sea", "bla", "black", "Bohemia", "boh", "Brest", "bre", "Budapest", "bud", "Bulgaria", "bul", "Burgundy", "bur", "Clyde", "cly", "Constantinople", "con", "Denmark", "den", "EasternMediterranean", "eas", "emed", "east", "eastmed", "ems", "eme", "Edinburgh", "edi", "EnglishChannel", "eng", "EnglishChannel", "ech", "Finland", "fin", "Galicia", "gal", "Gascony", "gas", "Greece", "gre", "GulfofLyon", "lyo", "gol", "gulfofl", "lyon", "GulfofBothnia", "bot", "gob", "both", "gulfofb", "bothnia", "HelgolandBight", "hel", "helgoland", "Holland", "hol", "Ionian Sea", "ion", "ionian", "Ireland", "ire", "Irish Sea", "iri", "irish", "Kiel", "kie", "Liverpool", "lvp", "livp", "lpl", "Livonia", "lvn", "livo", "lvo", "lva", "London", "lon", "Marseilles", "mar", "mars", "Mid-AtlanticOcean", "mao", "midatlantic", "mid", "mat", "Moscow", "mos", "Munich", "mun", "Naples", "nap", "napoli", "NorthAtlanticOcean", "nao", "nat", "NorthAfrica", "naf", "nora", "North Sea", "nth", "norsea", "nts", "Norway", "nor", "nwy", "norw", "Norwegian Sea", "nwg", "norwsea", "nrg", "norwegian", "Paris", "par", "Picardy", "pic", "Piedmont", "pie", "piemonte", "Portugal", "por", "Prussia", "pru", "Rome", "rom", "roma", "Ruhr", "ruh", "Rumania", "rum", "Serbia", "ser", "Sevastopol", "sev", "sevastapol", "Silesia", "sil", "Skagerrak", "ska", "Smyrna", "smy", "Spain", "spa", "StPetersburg", "stp", "Sweden", "swe", "Switzerlandswi", "switz", "Syria", "syr", "Trieste", "tri", "Tunis", "tun", "tunisia", "Tuscany", "tus", "Tyrolia", "tyr", "tyl", "trl", "Tyrrhenian Sea", "tys", "tyrr", "tyn", "tyh", "Ukraine", "ukr", "Venice", "ven", "venizia", "Vienna", "vie", "Wales", "wal", "Warsaw", "war", "WesternMediterranean", "wes", "wmed", "west", "western", "wms", "wme", "Yorkshire", "yor", "york", "yonkers" ]


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
        ( _, Error _, _) ->
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
            "country" ->
                { neededNext = [], currentStatus = Complete, stageCategory =  None }

            _ ->
                { neededNext = countries, currentStatus = Continue, stageCategory = Country }
    , \input ->
        case input of
            "country" ->
                { neededNext = [ "move", "->" ], currentStatus = Complete, stageCategory =Country }

            _ ->
                { neededNext = [ "move", "->" ], currentStatus = Continue, stageCategory = Command }
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
