module Hint exposing (StageStatus(..), getStageStatus)


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


getNeededStr : StageNeeds -> String
getNeededStr stageNeeds =
    stageNeeds.neededToContinue
        ++ stageNeeds.neededToComplete
        |> List.foldr (\a b -> a ++ ", " ++ b) " "


getNewStageStatus : String -> StageNeeds -> (String -> StageNeeds) -> StageStatus
getNewStageStatus input stageNeeds nextStageNeeds =
    if stageNeeds.neededToComplete |> List.member input then
        CanComplete (nextStageNeeds input)

    else if stageNeeds.neededToContinue |> List.member input then
        CanContinue (nextStageNeeds input)

    else
        Error ("Needed " ++ getNeededStr stageNeeds ++ " but found: " ++ input)


{-| This function is designed to be partially applied with `nextStageNeeds` and `input` being provided up front.

    Each word in the input is associated with a given evaluator, and the conditions required for the next stage of the evaluation.
    The conditions required for the current evaluator are passed through by the previous stage, either wrapped in a CanComplete or
    a CanContinue.

    If the criteria is not met then an Error and a reason is passed through.

    For example, if a given an evaluator requires `neededToContinue = ["continue"]` and `neededToComplete = ["complete"]`, the input must
    be either "word" or "complete". "complete" will pass a `CanComplete`, "continue" will pass a `CanContinue` to the next stage, which wraps the
    already applied criteria for the next stage.

-}
fullEvaluator : (String -> StageNeeds) -> String -> StageStatus -> StageStatus
fullEvaluator nextStageNeeds input previousStage =
    case previousStage of
        Error reason ->
            Error reason

        CanComplete stageNeeds ->
            getNewStageStatus input stageNeeds nextStageNeeds

        CanContinue stageNeeds ->
            getNewStageStatus input stageNeeds nextStageNeeds


stages : List (String -> StageNeeds)
stages =
    [ \_ -> { neededToContinue = [ "f", "a" ], neededToComplete = [] }
    , \_ -> { neededToContinue = [ "country" ], neededToComplete = [] }
    , \_ -> { neededToContinue = [ "move", "->", "supports" ], neededToComplete = [] }
    , \input ->
        case input of
            "supports" ->
                { neededToContinue = [ "f", "a" ], neededToComplete = [] }

            _ ->
                { neededToContinue = [], neededToComplete = [ "country" ] }
    , \input ->
        case input of
            "country" ->
                { neededToContinue = [], neededToComplete = [] }

            _ ->
                { neededToContinue = [], neededToComplete = [ "country" ] }

    , \_ -> { neededToContinue = [ "move", "->" ], neededToComplete = [] }
    , \_ -> { neededToContinue = [], neededToComplete = [ "country" ] }
    , \_ -> { neededToContinue = [], neededToComplete = [] }
    ]



{-
   Use `List String -> StageNeeds` instead
   parses input and returns correct stageNeeds

-}


getPartialEvaluators : List String -> List Evaluator
getPartialEvaluators input =
    List.map2 Tuple.pair stages input
        |> List.map (\( stageNeeds, str ) -> fullEvaluator stageNeeds str)


applyEvaluators : List Evaluator -> StageStatus
applyEvaluators evaluators =
    case evaluators of
        [] ->
            -- seed algorithm with empty base case
            CanContinue { neededToComplete = [], neededToContinue = [ "" ] }

        first :: rest ->
            first (applyEvaluators rest)


getStageStatus : String -> StageStatus
getStageStatus str =
    ""
        :: String.words str
        |> getPartialEvaluators
        |> List.reverse
        |> applyEvaluators



-- bug: needed to complete and needed to continue are not always in sync
-- e.g. f country supports country country
-- back to the drawing board
