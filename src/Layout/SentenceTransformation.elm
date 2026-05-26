module Layout.SentenceTransformation exposing (view)

import Domain.Question exposing (Question, SentenceTransformationQuestion)
import Html exposing (Html, p, text)
import Html.Attributes exposing (style)
import Layout.Primitives exposing (card, instructionText, sectionTitle)
import Layout.QuestionForm exposing (highlightBox, textResponse)


view : String -> (String -> msg) -> Question -> SentenceTransformationQuestion -> Html msg
view currentText onTextChange question payload =
    card
        [ sectionTitle question.title
        , p [] [ text question.prompt ]
        , instructionLine question.instructions
        , highlightBox ("Frase-base: " ++ payload.sourceSentence)
        , p [ style "margin-top" "10px", style "font-weight" "600" ] [ text ("Transformação: " ++ payload.transformationRule) ]
        , textResponse 4 Nothing currentText onTextChange
        ]


instructionLine : Maybe String -> Html msg
instructionLine instructions =
    case instructions of
        Just value ->
            instructionText value

        Nothing ->
            instructionText "Reescreva a frase conforme a transformação solicitada."
