module Layout.OralInteractive exposing (view)

import Domain.Question exposing (OralInteractiveQuestion, Question)
import Html exposing (Html, li, p, text, ul)
import Html.Attributes exposing (style)
import Layout.Primitives exposing (card, sectionTitle)
import Layout.QuestionForm exposing (instructionLine, textResponse)


view : { transcript : String, notes : String } -> (String -> msg) -> (String -> msg) -> Question -> OralInteractiveQuestion -> Html msg
view answers onTranscriptChange onNotesChange question data =
    card
        [ sectionTitle question.title
        , p [ style "margin" "0 0 10px" ] [ text question.prompt ]
        , instructionLine question.instructions question.prompt "Simule a interação oral e registre o resultado."
        , p [ style "margin" "0 0 6px" ] [ text ("Cenário: " ++ data.scenario) ]
        , p [ style "margin" "0 0 10px" ] [ text ("Papel: " ++ data.role) ]
        , expectedPointsList data.expectedPoints
        , p [ style "margin" "10px 0 4px" ] [ text "Transcrição" ]
        , textResponse 4 (Just "Digite a fala principal") answers.transcript onTranscriptChange
        , p [ style "margin" "10px 0 4px" ] [ text "Notas de desempenho" ]
        , textResponse 3 (Just "Registre pontos fortes e melhorias") answers.notes onNotesChange
        ]


expectedPointsList : Maybe (List String) -> Html msg
expectedPointsList maybePoints =
    case maybePoints of
        Just points ->
            ul [ style "margin" "0 0 10px", style "padding-left" "20px" ]
                (List.map (\item -> li [] [ text item ]) points)

        Nothing ->
            text ""
