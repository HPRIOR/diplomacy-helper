module Hint exposing (getStageStatus, StageStatus(..))
type StageStatus
    = CanComplete StageNeeds
    | CanContinue StageNeeds
    | Error String


type alias StageNeeds =
    { neededToContinue : List String
    , neededToComplete : List String
    }


type alias Evaluator =
    StageStatus -> StageStatus

getNeededStr: StageNeeds -> String
getNeededStr stageNeeds =
    stageNeeds.neededToContinue ++ stageNeeds.neededToComplete
    |> List.foldr (\a b -> a ++ ", " ++ b) " "


getNewStageStatus : String -> StageNeeds -> StageNeeds -> StageStatus
getNewStageStatus input stageNeeds nextStageNeeds =
    if List.member input stageNeeds.neededToComplete then
        CanComplete nextStageNeeds

    else if List.member input stageNeeds.neededToContinue then
        CanContinue nextStageNeeds

    else
        Error ("Needed " ++ getNeededStr stageNeeds ++ " but found: " ++ input)


evalStage : StageNeeds -> String -> StageStatus -> StageStatus
evalStage nextStageNeeds input previousStage =
    case previousStage of
        Error reason ->
            Error reason

        CanComplete stageNeeds ->
            getNewStageStatus input stageNeeds nextStageNeeds

        CanContinue stageNeeds ->
            getNewStageStatus input stageNeeds nextStageNeeds


stages : List StageNeeds
stages =
    [ { neededToContinue = [ "f", "a" ], neededToComplete = [] }
    , { neededToContinue = [ "country" ], neededToComplete = [] }
    , { neededToContinue = [ "move", "->", "supports" ], neededToComplete = [] }
    , { neededToContinue = [ "f", "a" ], neededToComplete = [ "country" ] }
    , { neededToContinue = [], neededToComplete = [ "country" ] }
    , { neededToContinue = [ "move", "->" ], neededToComplete = [] }
    , { neededToContinue = [], neededToComplete = [ "country" ] }
    ]


getEvaluators : List String -> List Evaluator
getEvaluators input =
    List.map2 Tuple.pair stages input
        |> List.map (\( stageNeeds, str ) -> evalStage stageNeeds str)


applyEvaluators : List Evaluator -> StageStatus
applyEvaluators evaluators =
    case evaluators of
        [] -> 
            CanContinue { neededToComplete = [], neededToContinue = [""]}

        (first::rest) ->
            first (applyEvaluators rest)


getStageStatus : String -> StageStatus
getStageStatus str =
    ""::(String.split " " str)
        |> getEvaluators
        |> List.reverse
        |> applyEvaluators
        
