module Layout.OpenQuestion exposing (view)

import Domain.Question exposing (OpenQuestionData, Question)
import Html exposing (Html, p, text)
import Layout.Primitives exposing (card, instructionText, sectionTitle)
import Layout.QuestionForm exposing (textResponse)


view : String -> (String -> msg) -> Question -> OpenQuestionData -> Html msg
view currentText onTextChange question data =
    card
        [ sectionTitle question.title
        , p [] [ text question.prompt ]
        , instructionLine question.instructions
        , textResponse 6 (Just data.placeholder) currentText onTextChange
        ]


instructionLine : Maybe String -> Html msg
instructionLine instructions =
    case instructions of
        Just value ->
            instructionText value

        Nothing ->
            instructionText "Escreva uma resposta objetiva e clara."
