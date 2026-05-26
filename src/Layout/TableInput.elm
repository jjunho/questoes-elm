module Layout.TableInput exposing (view)

import Dict exposing (Dict)
import Domain.Question exposing (Question, TableInputQuestion)
import Html exposing (Html, p, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (style)
import Layout.Primitives exposing (card, sectionTitle)
import Layout.QuestionForm exposing (instructionLine, textResponse)


view : Dict Int (Dict Int String) -> (Int -> Int -> String -> msg) -> Question -> TableInputQuestion -> Html msg
view answers onCellChange question data =
    card
        [ sectionTitle question.title
        , p [ style "margin" "0 0 10px" ] [ text question.prompt ]
        , instructionLine question.instructions question.prompt "Preencha os campos da tabela."
        , table
            [ style "width" "100%"
            , style "border-collapse" "collapse"
            , style "margin-top" "10px"
            ]
            [ thead [] [ headerRow data.columns ]
            , tbody [] (List.map (rowView answers onCellChange data.columns) (List.range 0 (data.rowCount - 1)))
            ]
        ]


headerRow : List String -> Html msg
headerRow columns =
    tr [] (List.map headerCell columns)


headerCell : String -> Html msg
headerCell columnName =
    th
        [ style "text-align" "left"
        , style "padding" "8px"
        , style "border-bottom" "1px solid #cbd5e1"
        ]
        [ text columnName ]


rowView : Dict Int (Dict Int String) -> (Int -> Int -> String -> msg) -> List String -> Int -> Html msg
rowView answers onCellChange columns rowIndex =
    tr [] (List.indexedMap (cellView answers onCellChange rowIndex) columns)


cellView : Dict Int (Dict Int String) -> (Int -> Int -> String -> msg) -> Int -> Int -> String -> Html msg
cellView answers onCellChange rowIndex columnIndex _ =
    td [ style "padding" "6px" ]
        [ textResponse
            1
            Nothing
            (cellValue answers rowIndex columnIndex)
            (onCellChange rowIndex columnIndex)
        ]


cellValue : Dict Int (Dict Int String) -> Int -> Int -> String
cellValue answers rowIndex columnIndex =
    Dict.get rowIndex answers
        |> Maybe.andThen (Dict.get columnIndex)
        |> Maybe.withDefault ""
