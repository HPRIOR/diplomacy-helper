module Client exposing (main)

import Browser
import Hint exposing (hint)
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onInput)


type alias Model =
    { textArea : String }


type Msg
    = TextAreaChange String



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "flex flex-col items-center" ]
        [ h1 [ class "text-3xl underline" ] [ text "hello world" ]
        , input [ class "mt-5 mb-5", onInput TextAreaChange ] []
        , div [] [ text model.textArea ]
        ]



-- UDPATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TextAreaChange str ->
            ( { model | textArea = hint str }, Cmd.none )



-- MODEL


initModel : Model
initModel =
    { textArea = "" }


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
