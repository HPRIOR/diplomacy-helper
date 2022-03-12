module Hint exposing (hint)

import Dict exposing (get)
import Html exposing (input)


type StageStatus
    = CanComplete StageNeeds
    | CanContinue StageNeeds
    | Error String


type alias StageNeeds =
    { neededToContinue : List String
    , neededToComplete : List String
    }


getNewStageStatus : String -> StageNeeds -> StageNeeds -> StageStatus
getNewStageStatus input stageNeeds nextStageNeeds =
    if List.member input stageNeeds.neededToComplete then
        CanComplete nextStageNeeds

    else if List.member input stageNeeds.neededToContinue then
        CanContinue nextStageNeeds

    else
        Error "Needed something but didn't find anything"


evalStage : StageStatus -> String -> StageStatus
evalStage previousStage input =
    case previousStage of
        Error reason ->
            Error reason

        CanComplete neededToContinue neededToComplete ->
            getNewStageStatus input neededToContinue neededToComplete [] []

        CanContinue neededToContinue neededToComplete ->
            getNewStageStatus input neededToContinue neededToComplete [] []


hint : String -> String
hint str =
    str
