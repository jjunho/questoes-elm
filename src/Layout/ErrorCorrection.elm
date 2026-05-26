module Layout.ErrorCorrection exposing (view)

import Domain.Question exposing (Question)
import Html exposing (Html, p, text)
import Layout.Primitives exposing (card, instructionText, sectionTitle)
import Layout.QuestionForm exposing (highlightBox, textResponse)


view : String -> (String -> msg) -> Question -> { sentence : String, expectedCorrection : Maybe String } -> Html msg
view currentText onTextChange question payload =
    card
        [ sectionTitle question.title
        , p [] [ text question.prompt ]
        , instructionLine question.instructions
        , highlightBox payload.sentence
        , textResponse 4 Nothing currentText onTextChange
        ]


instructionLine : Maybe String -> Html msg
instructionLine instructions =
    case instructions of
        Just value ->
            instructionText value

        Nothing ->
            instructionText "Reescreva a frase corrigindo os erros."
