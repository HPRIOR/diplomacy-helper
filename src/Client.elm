module Client exposing (main, removeLastWord)

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


hintInInput : String -> String -> Bool
hintInInput input hint =
    let
        lastInput =
            case input |> String.words |> List.reverse of
                head :: _ ->
                    head

                _ ->
                    ""
    in
    hint |> String.startsWith lastInput


getHint : String -> List String -> List String
getHint input neededNext =
    neededNext |> List.map (\needed -> input ++ " " ++ needed)


removeLastWord : String -> String
removeLastWord str =
    let
        lastWordLen =
            case str |> String.words |> List.reverse of
                head :: _ ->
                    String.length head

                _ ->
                    0
    in
    str |> String.left (String.length str - lastWordLen - 1)


getInputWithoutLastWord : String -> List String -> List String
getInputWithoutLastWord input neededNext =
    neededNext |> List.map (\needed -> removeLastWord input ++ " " ++ needed)


lastInputStartsWithHint : String -> List String -> Bool
lastInputStartsWithHint input neededNext =
    let
        maybeLastInput =
            input |> String.words |> List.reverse |> List.head
    in
    case maybeLastInput of
        Nothing ->
            False

        Just lastInput ->
            neededNext |> List.any (\needed -> String.startsWith lastInput needed)


{-| Appends the current input with any required input needed to progress to the next stage.
-}
getHints : StageNeeds -> String -> List String
getHints stageNeeds input =
    if lastInputStartsWithHint input stageNeeds.neededNext then
        stageNeeds.neededNext
            |> List.filter (hintInInput input)
            |> getInputWithoutLastWord input

    else
        case stageNeeds.currentStatus of
            Error _ ->
                []

            _ ->
                getHint input stageNeeds.neededNext


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
                    getHints model.stageNeeds model.input
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
