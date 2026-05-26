module Layout.AnnotatedEdit exposing (view)

import Domain.Question exposing (AnnotatedEditQuestion, Question)
import Html exposing (Html, p, text)
import Html.Attributes exposing (style)
import Layout.Primitives exposing (card, sectionTitle)
import Layout.QuestionForm exposing (highlightBox, instructionLine, textResponse)


view : { highlightedText : String, correction : String } -> (String -> msg) -> (String -> msg) -> Question -> AnnotatedEditQuestion -> Html msg
view answers onHighlightChange onCorrectionChange question data =
    card
        [ sectionTitle question.title
        , p [ style "margin" "0 0 10px" ] [ text question.prompt ]
        , instructionLine question.instructions question.prompt "Selecione o trecho e escreva a correção sugerida."
        , highlightBox data.sourceText
        , p [ style "margin" "10px 0 4px" ] [ text "Trecho destacado" ]
        , textResponse 2 (Just "Ex.: verbo no tempo incorreto") answers.highlightedText onHighlightChange
        , p [ style "margin" "10px 0 4px" ] [ text "Correção" ]
        , textResponse 3 (Just "Escreva a versão corrigida") answers.correction onCorrectionChange
        ]
