module Layout.OddOneOut exposing (view)

import Domain.Question exposing (OddOneOutQuestion, Question)
import Html exposing (Html, p, text)
import Html.Attributes exposing (style)
import Layout.Primitives exposing (card, instructionText, sectionTitle)
import Layout.QuestionForm exposing (choiceList)


view : Maybe Int -> (Int -> msg) -> Question -> OddOneOutQuestion -> Html msg
view maybeSelectedIndex onSelectOption question data =
    card
        [ sectionTitle question.title
        , p [ style "margin" "0 0 10px" ] [ text question.prompt ]
        , instructionLine question.instructions
        , choiceList maybeSelectedIndex onSelectOption data.options
        ]


instructionLine : Maybe String -> Html msg
instructionLine instructions =
    case instructions of
        Just value ->
            instructionText value

        Nothing ->
            instructionText "Selecione a palavra intrusa."
