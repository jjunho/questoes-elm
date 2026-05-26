module Layout.Classification exposing (view)

import Dict exposing (Dict)
import Domain.Question exposing (ClassificationQuestion, Question)
import Html exposing (Html, div, option, p, select, span, text)
import Html.Attributes exposing (style, value)
import Html.Events exposing (onInput)
import Layout.Primitives exposing (card, sectionTitle)
import Layout.QuestionForm exposing (instructionLine)
import String


view : Dict Int Int -> (Int -> Maybe Int -> msg) -> Question -> ClassificationQuestion -> Html msg
view answers onClassify question data =
    card
        [ sectionTitle question.title
        , p [ style "margin" "0 0 10px" ] [ text question.prompt ]
        , instructionLine question.instructions question.prompt "Classifique cada item na categoria correta."
        , div [ style "display" "grid", style "gap" "10px" ]
            (List.indexedMap (itemRow answers onClassify data.labels) data.items)
        ]


itemRow : Dict Int Int -> (Int -> Maybe Int -> msg) -> List String -> Int -> String -> Html msg
itemRow answers onClassify labels index itemText =
    div
        [ style "display" "grid"
        , style "grid-template-columns" "1fr 220px"
        , style "gap" "10px"
        , style "align-items" "center"
        ]
        [ span [ style "padding" "8px 0" ] [ text itemText ]
        , select
            [ value (selectedValue answers index)
            , onInput (selectionToMessage index onClassify)
            , style "padding" "8px"
            , style "border" "1px solid #cbd5e1"
            , style "border-radius" "8px"
            ]
            (option [ value "" ] [ text "Selecione" ]
                :: List.indexedMap labelOption labels
            )
        ]


selectedValue : Dict Int Int -> Int -> String
selectedValue answers index =
    Dict.get index answers
        |> Maybe.map String.fromInt
        |> Maybe.withDefault ""


selectionToMessage : Int -> (Int -> Maybe Int -> msg) -> String -> msg
selectionToMessage itemIndex onClassify rawValue =
    if rawValue == "" then
        onClassify itemIndex Nothing

    else
        case String.toInt rawValue of
            Just labelIndex ->
                onClassify itemIndex (Just labelIndex)

            Nothing ->
                onClassify itemIndex Nothing


labelOption : Int -> String -> Html msg
labelOption index labelText =
    option [ value (String.fromInt index) ] [ text labelText ]
