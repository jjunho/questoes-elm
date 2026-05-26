module Layout.QuestionForm exposing (choiceList, highlightBox, instructionLine, textResponse)

import Html exposing (Attribute, Html, button, li, ol, p, text, textarea)
import Html.Attributes exposing (placeholder, rows, style, value)
import Html.Events exposing (onClick, onInput)
import Layout.Primitives exposing (instructionText)
import String


instructionLine : Maybe String -> String -> String -> Html msg
instructionLine maybeInstructions prompt defaultText =
    case maybeInstructions of
        Just value ->
            if normalized value == normalized prompt then
                text ""

            else
                instructionText value

        Nothing ->
            instructionText defaultText


choiceList : Maybe Int -> (Int -> msg) -> List String -> Html msg
choiceList maybeSelectedIndex onSelect options =
    ol [ style "margin" "0", style "padding-left" "20px" ]
        (List.indexedMap (choiceItem maybeSelectedIndex onSelect) options)


choiceItem : Maybe Int -> (Int -> msg) -> Int -> String -> Html msg
choiceItem maybeSelectedIndex onSelect index optionLabel =
    let
        isSelected =
            maybeSelectedIndex == Just index
    in
    li [ style "margin-bottom" "6px" ]
        [ button
            [ onClick (onSelect index)
            , style "width" "100%"
            , style "text-align" "left"
            , style "padding" "10px 12px"
            , style "border-radius" "8px"
            , style "border"
                (if isSelected then
                    "2px solid #1d4ed8"

                 else
                    "1px solid #cbd5e1"
                )
            , style "background"
                (if isSelected then
                    "#dbeafe"

                 else
                    "#ffffff"
                )
            , style "cursor" "pointer"
            ]
            [ text optionLabel ]
        ]


highlightBox : String -> Html msg
highlightBox value_ =
    p
        [ style "padding" "10px"
        , style "background" "#f8fafc"
        , style "border" "1px solid #e2e8f0"
        , style "border-radius" "8px"
        ]
        [ text value_ ]


textResponse : Int -> Maybe String -> String -> (String -> msg) -> Html msg
textResponse rowCount maybePlaceholder currentValue onTextChange =
    textarea
        ([ rows rowCount
         , value currentValue
         , onInput onTextChange
         , style "width" "100%"
         , style "margin-top" "10px"
         , style "padding" "10px"
         , style "border-radius" "8px"
         , style "border" "1px solid #cbd5e1"
         ]
            ++ placeholderAttribute maybePlaceholder
        )
        []


placeholderAttribute : Maybe String -> List (Attribute msg)
placeholderAttribute maybeValue =
    case maybeValue of
        Just value_ ->
            [ placeholder value_ ]

        Nothing ->
            []


normalized : String -> String
normalized value_ =
    String.trim value_
        |> String.toLower
