module Client exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class)


type alias Model =
    { something : String }


type Msg
    = Something



-- VIEW


view : Model -> Html msg
view _ =
    h1 [ class "centre text-3xl font-bold underline"] [ text "hello" ]



-- UDPATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Something ->
            ( model, Cmd.none )



-- MODEL


initModel : Model
initModel =
    { something = "hello" }


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
