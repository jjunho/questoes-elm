module Layout.Ordering exposing (view)

import Dict exposing (Dict)
import Domain.Question exposing (Question)
import Html exposing (Html, li, option, p, select, text, ul)
import Html.Attributes exposing (selected, style, value)
import Html.Events exposing (onInput)
import Layout.Primitives exposing (card, instructionText, sectionTitle)


view : Dict Int Int -> (Int -> Maybe Int -> msg) -> Question -> { items : List String, correctOrder : Maybe (List Int) } -> Html msg
view selectedOrder onSelectOrder question payload =
    let
        totalItems =
            List.length payload.items
    in
    card
        [ sectionTitle question.title
        , p [] [ text question.prompt ]
        , instructionLine question.instructions
        , ul [ style "margin" "0", style "padding-left" "20px" ]
            (List.indexedMap (orderingItem selectedOrder onSelectOrder totalItems) payload.items)
        ]


instructionLine : Maybe String -> Html msg
instructionLine instructions =
    case instructions of
        Just value ->
            instructionText value

        Nothing ->
            instructionText "Defina a posição correta de cada item."


orderingItem : Dict Int Int -> (Int -> Maybe Int -> msg) -> Int -> Int -> String -> Html msg
orderingItem selectedOrder onSelectOrder totalItems itemIndex itemLabel =
    let
        maybeSelected =
            Dict.get itemIndex selectedOrder
    in
    li [ style "margin-bottom" "8px" ]
        [ text itemLabel
        , select
            [ onInput (\rawSelection -> onSelectOrder itemIndex (toMaybeInt rawSelection))
            , style "margin-left" "8px"
            , style "padding" "6px 8px"
            , style "border-radius" "6px"
            , style "border" "1px solid #cbd5e1"
            ]
            (option [ value "", selected (maybeSelected == Nothing) ] [ text "Posição" ]
                :: List.map (positionOption maybeSelected) (List.range 1 totalItems)
            )
        ]


positionOption : Maybe Int -> Int -> Html msg
positionOption maybeSelected position =
    option
        [ value (String.fromInt position)
        , selected (maybeSelected == Just position)
        ]
        [ text (String.fromInt position) ]


toMaybeInt : String -> Maybe Int
toMaybeInt rawValue =
    if rawValue == "" then
        Nothing

    else
        String.toInt rawValue
