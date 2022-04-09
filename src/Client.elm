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


viewSubmitButton : StageNeeds -> Html Msg
viewSubmitButton stageNeeds =
    case stageNeeds.currentStatus of
        Complete ->
            button [] [ text "Can Submit" ]

        _ ->
            button [] [ text "Can't Submit" ]


{-| Appends the current input with any required input needed to progress to the next stage. 
-}
getHints : List String -> String -> List String
getHints needed input =
    if String.length input /= 0 then
        needed |> List.map (\s -> input ++ " " ++ s)
    else needed


{-| Converts suggested inputs to html
-}
viewHints : List String -> List (Html Msg)
viewHints hints =
    hints
        |> List.map (\hint -> div [] [ text hint ])
        |> List.take 10


view : Model -> Html Msg
view model =
    div [ class "flex flex-col items-center" ]
        [ h1 [ class "text-3xl" ] [ text "Diplomacy Helper!" ]
        , div []
            [ input [ class "mt-5 mb-5", onInput TextAreaChange ] []
            , viewSubmitButton model.stageNeeds
            ]
        , div [] <|
            let
                hints =
                    getHints model.stageNeeds.neededNext model.input
            in
            viewHints hints
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
        { neededNext = [ "f", "a" ], currentStatus = Continue, stageCategory = UnitType }
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
