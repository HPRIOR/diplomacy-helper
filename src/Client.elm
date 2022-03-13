module Client exposing (main)

import Browser
import Hint exposing (..)
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onInput)


type alias Model =
    { stageStatus : StageStatus
    , input : String
    }


type Msg
    = TextAreaChange String



-- VIEW


getSuggestions : List String -> String -> List String
getSuggestions needed input =
    needed |> List.map (\s -> input ++ " " ++ s)






stageStatusInterpreter : StageStatus -> String -> List String
stageStatusInterpreter stageStatus input =
    case stageStatus of
        CanComplete stageNeeds ->
            getSuggestions (stageNeeds.neededToContinue ++ stageNeeds.neededToComplete) input

        CanContinue stageNeeds ->
            getSuggestions (stageNeeds.neededToContinue ++ stageNeeds.neededToComplete) input

        Error e ->
            [ e ]


viewStageStatus : List String -> List (Html Msg)
viewStageStatus hints =
    hints |> List.map (\hint -> div [] [ text hint ])


viewSubmitButton : StageStatus -> Html Msg
viewSubmitButton stageStatus =
    case stageStatus of
        CanComplete _ ->
            button [] [ text "Can Submit" ]

        _ ->
            button [] [ text "Can't Submit" ]


view : Model -> Html Msg
view model =
    div [ class "flex flex-col items-center" ]
        [ h1 [ class "text-3xl underline" ] [ text "hello world" ]
        , div []
            [ input [ class "mt-5 mb-5", onInput TextAreaChange ] []
            , viewSubmitButton model.stageStatus
            ]
        , div [] (viewStageStatus (stageStatusInterpreter model.stageStatus model.input))
        ]



-- UDPATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TextAreaChange input ->
            ( { model | stageStatus = getStageStatus input, input = input }, Cmd.none )



-- MODEL


initModel : Model
initModel =
    { stageStatus = CanContinue { neededToComplete = [], neededToContinue = [] }
    , input = ""
    }


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
