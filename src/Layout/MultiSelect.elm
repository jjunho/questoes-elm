module Layout.MultiSelect exposing (view)

import Dict exposing (Dict)
import Domain.Question exposing (MultiSelectQuestion, Question)
import Html exposing (Html, div, input, label, p, text)
import Html.Attributes exposing (checked, style, type_)
import Html.Events exposing (onCheck)
import Layout.Primitives exposing (card, sectionTitle)
import Layout.QuestionForm exposing (instructionLine)
import String


view : Dict Int Bool -> (Int -> Bool -> msg) -> Question -> MultiSelectQuestion -> Html msg
view answers onToggle question data =
    card
        [ sectionTitle question.title
        , p [ style "margin" "0 0 10px" ] [ text question.prompt ]
        , instructionLine question.instructions question.prompt "Selecione uma ou mais alternativas."
        , selectionHint data
        , div [ style "display" "grid", style "gap" "8px" ]
            (List.indexedMap (optionRow answers onToggle) data.options)
        ]


selectionHint : MultiSelectQuestion -> Html msg
selectionHint data =
    case ( data.minSelections, data.maxSelections ) of
        ( Just min_, Just max_ ) ->
            p [ style "margin" "0 0 10px", style "color" "#475569" ]
                [ text ("Selecione entre " ++ String.fromInt min_ ++ " e " ++ String.fromInt max_ ++ " opções.") ]

        ( Just min_, Nothing ) ->
            p [ style "margin" "0 0 10px", style "color" "#475569" ]
                [ text ("Selecione no mínimo " ++ String.fromInt min_ ++ " opções.") ]

        ( Nothing, Just max_ ) ->
            p [ style "margin" "0 0 10px", style "color" "#475569" ]
                [ text ("Selecione no máximo " ++ String.fromInt max_ ++ " opções.") ]

        ( Nothing, Nothing ) ->
            text ""


optionRow : Dict Int Bool -> (Int -> Bool -> msg) -> Int -> String -> Html msg
optionRow answers onToggle index optionLabel =
    label
        [ style "display" "flex"
        , style "align-items" "center"
        , style "gap" "8px"
        , style "padding" "8px 10px"
        , style "border" "1px solid #cbd5e1"
        , style "border-radius" "8px"
        ]
        [ input
            [ type_ "checkbox"
            , checked (Dict.get index answers |> Maybe.withDefault False)
            , onCheck (onToggle index)
            ]
            []
        , text optionLabel
        ]
