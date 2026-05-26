module Layout.TrueFalse exposing (view)

import Domain.Question exposing (Question)
import Html exposing (Html, button, div, p, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Layout.Primitives exposing (card, instructionText, sectionTitle)


view : Maybe Bool -> (Bool -> msg) -> Question -> Html msg
view maybeSelected onSelect question =
    card
        [ sectionTitle question.title
        , p [] [ text question.prompt ]
        , instructionLine question.instructions
        , buttonsView maybeSelected onSelect
        ]


instructionLine : Maybe String -> Html msg
instructionLine instructions =
    case instructions of
        Just value ->
            instructionText value

        Nothing ->
            instructionText "Marque verdadeiro ou falso."


buttonsView : Maybe Bool -> (Bool -> msg) -> Html msg
buttonsView maybeSelected onSelect =
    div [ style "display" "flex", style "gap" "8px" ]
        [ answerButton "Verdadeiro" True maybeSelected onSelect
        , answerButton "Falso" False maybeSelected onSelect
        ]


answerButton : String -> Bool -> Maybe Bool -> (Bool -> msg) -> Html msg
answerButton label value maybeSelected onSelect =
    let
        isSelected =
            maybeSelected == Just value
    in
    button
        [ onClick (onSelect value)
        , style "padding" "10px 14px"
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
        [ text label ]
