module Client exposing (main)

import Browser
import Hint exposing (..)
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onInput)


type alias Model =
    { stageNeeds : StageNeeds
    , input : String
    }


type Msg
    = TextAreaChange String



-- VIEW


getSuggestions : List String -> String -> List String
getSuggestions needed input =
    needed |> List.map (\s -> input ++ " " ++ s)


getNeededStr : List String -> String
getNeededStr needed =
    needed
        |> List.foldr (\a b -> a ++ ", " ++ b) " "


stageStatusInterpreter : StageNeeds -> String -> List String
stageStatusInterpreter stageNeeds input =
    case ( stageNeeds.stageCode, stageNeeds.neededNext ) of
        ( Error, needed ) ->
            [ getNeededStr needed ]

        ( _, needed ) ->
            getSuggestions needed input


viewStageStatus : List String -> List (Html Msg)
viewStageStatus hints =
    hints |> List.map (\hint -> div [] [ text hint ])


viewSubmitButton : StageNeeds -> Html Msg
viewSubmitButton stageNeeds =
    case stageNeeds.stageCode of
        Complete ->
            button [] [ text "Can Submit" ]

        _ ->
            button [] [ text "Can't Submit" ]


view : Model -> Html Msg
view model =
    div [ class "flex flex-col items-center" ]
        [ h1 [ class "text-3xl" ] [ text "Diplomacy Helper!" ]
        , div []
            [ input [ class "mt-5 mb-5", onInput TextAreaChange ] []
            , viewSubmitButton model.stageNeeds
            ]
        , div [] (viewStageStatus (stageStatusInterpreter model.stageNeeds model.input))
        ]



-- UDPATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TextAreaChange input ->
            ( { model | stageNeeds = getStageStatus input, input = input }, Cmd.none )



-- MODEL


initModel : Model
initModel =
    { stageNeeds =
        { neededNext = [ "f", "a" ], stageCode = Continue }
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
