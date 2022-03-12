module Client exposing (main)

import Browser
import Hint exposing (..)
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onInput)


type alias Model =
    { parseResult : String }


type Msg
    = TextAreaChange String



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "flex flex-col items-center" ]
        [ h1 [ class "text-3xl underline" ] [ text "hello world" ]
        , input [ class "mt-5 mb-5", onInput TextAreaChange ] []
        , div [] [ text model.parseResult ]
        ]



-- UDPATE


stageStatusInterpreter : StageStatus -> String
stageStatusInterpreter stageStatus =
    case stageStatus of
        CanComplete _ ->
            "Can Complete"
        CanContinue _ ->
            "Can Continue"
        Error e ->
            e


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TextAreaChange str ->
            ( { model | parseResult = stageStatusInterpreter (getStageStatus str) }, Cmd.none )



-- MODEL


initModel : Model
initModel =
    { parseResult = "" }


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
