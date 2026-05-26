module Layout.DragReorder exposing (view)

import Dict exposing (Dict)
import Domain.Question exposing (DragReorderQuestion, Question)
import Html exposing (Html, div, option, p, select, span, text)
import Html.Attributes exposing (style, value)
import Html.Events exposing (onInput)
import Layout.Primitives exposing (card, sectionTitle)
import Layout.QuestionForm exposing (instructionLine)
import String


view : Dict Int Int -> (Int -> Maybe Int -> msg) -> Question -> DragReorderQuestion -> Html msg
view answers onSetPosition question data =
    card
        [ sectionTitle question.title
        , p [ style "margin" "0 0 10px" ] [ text question.prompt ]
        , instructionLine question.instructions question.prompt "Defina a posição de cada item na ordem final."
        , p [ style "margin" "0 0 10px", style "color" "#475569" ] [ text "Forma de arraste representada por seleção de posições para manter compatibilidade e simplicidade." ]
        , div [ style "display" "grid", style "gap" "10px" ]
            (List.indexedMap (itemRow answers onSetPosition (List.length data.items)) data.items)
        ]


itemRow : Dict Int Int -> (Int -> Maybe Int -> msg) -> Int -> Int -> String -> Html msg
itemRow answers onSetPosition totalPositions itemIndex itemText =
    div
        [ style "display" "grid"
        , style "grid-template-columns" "1fr 140px"
        , style "gap" "10px"
        , style "align-items" "center"
        ]
        [ span [ style "padding" "8px 0" ] [ text itemText ]
        , select
            [ value (selectedValue answers itemIndex)
            , onInput (selectionToMessage itemIndex onSetPosition)
            , style "padding" "8px"
            , style "border" "1px solid #cbd5e1"
            , style "border-radius" "8px"
            ]
            (option [ value "" ] [ text "Posição" ]
                :: List.map positionOption (List.range 1 totalPositions)
            )
        ]


selectedValue : Dict Int Int -> Int -> String
selectedValue answers index =
    Dict.get index answers
        |> Maybe.map String.fromInt
        |> Maybe.withDefault ""


selectionToMessage : Int -> (Int -> Maybe Int -> msg) -> String -> msg
selectionToMessage itemIndex onSetPosition rawValue =
    if rawValue == "" then
        onSetPosition itemIndex Nothing

    else
        case String.toInt rawValue of
            Just position ->
                onSetPosition itemIndex (Just position)

            Nothing ->
                onSetPosition itemIndex Nothing


positionOption : Int -> Html msg
positionOption position =
    option [ value (String.fromInt position) ] [ text (String.fromInt position) ]
