module Layout.Translation exposing (view)

import Domain.Question exposing (Question, TranslationQuestion)
import Html exposing (Html, p, text)
import Html.Attributes exposing (style)
import Layout.Primitives exposing (card, instructionText, sectionTitle)
import Layout.QuestionForm exposing (highlightBox, textResponse)


view : String -> (String -> msg) -> Question -> TranslationQuestion -> Html msg
view currentText onTextChange question payload =
    card
        [ sectionTitle question.title
        , p [] [ text question.prompt ]
        , instructionLine question.instructions
        , p [ style "font-size" "0.95rem", style "color" "#475569", style "margin-bottom" "6px" ]
            [ text ("Traduza de " ++ payload.sourceLang ++ " para " ++ payload.targetLang ++ ".") ]
        , highlightBox payload.sourceText
        , textResponse 4 Nothing currentText onTextChange
        ]


instructionLine : Maybe String -> Html msg
instructionLine instructions =
    case instructions of
        Just value ->
            instructionText value

        Nothing ->
            instructionText "Faça a tradução mantendo o sentido do texto."
