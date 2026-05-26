module Layout.Matching exposing (view)

import Dict exposing (Dict)
import Domain.Question exposing (MatchingPair, Question)
import Html exposing (Html, li, option, p, select, text, ul)
import Html.Attributes exposing (selected, style, value)
import Html.Events exposing (onInput)
import Layout.Primitives exposing (card, instructionText, sectionTitle)


view : Dict Int Int -> (Int -> Maybe Int -> msg) -> Question -> { pairs : List MatchingPair } -> Html msg
view selectedPairs onSelectPair question payload =
    let
        rightOptions =
            List.map .right payload.pairs
    in
    card
        [ sectionTitle question.title
        , p [] [ text question.prompt ]
        , instructionLine question.instructions
        , ul [ style "margin" "0", style "padding-left" "20px" ] (List.indexedMap (pairItem selectedPairs onSelectPair rightOptions) payload.pairs)
        ]


instructionLine : Maybe String -> Html msg
instructionLine instructions =
    case instructions of
        Just value ->
            instructionText value

        Nothing ->
            instructionText "Associe os itens corretamente."


pairItem : Dict Int Int -> (Int -> Maybe Int -> msg) -> List String -> Int -> MatchingPair -> Html msg
pairItem selectedPairs onSelectPair rightOptions leftIndex pair =
    let
        maybeSelectedRight =
            Dict.get leftIndex selectedPairs
    in
    li [ style "margin-bottom" "8px" ]
        [ text (pair.left ++ " -> ")
        , select
            [ onInput (\rawSelection -> onSelectPair leftIndex (toMaybeInt rawSelection))
            , style "margin-left" "8px"
            , style "padding" "6px 8px"
            , style "border-radius" "6px"
            , style "border" "1px solid #cbd5e1"
            ]
            (option [ value "", selected (maybeSelectedRight == Nothing) ] [ text "Selecione" ]
                :: List.indexedMap (rightOption maybeSelectedRight) rightOptions
            )
        ]


rightOption : Maybe Int -> Int -> String -> Html msg
rightOption maybeSelectedRight index label =
    option
        [ value (String.fromInt index)
        , selected (maybeSelectedRight == Just index)
        ]
        [ text label ]


toMaybeInt : String -> Maybe Int
toMaybeInt rawValue =
    if rawValue == "" then
        Nothing

    else
        String.toInt rawValue
