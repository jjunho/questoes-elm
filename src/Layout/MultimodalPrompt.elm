module Layout.MultimodalPrompt exposing (view)

import Domain.Question exposing (MultimodalPromptQuestion, Question)
import Html exposing (Html, a, audio, p, source, text)
import Html.Attributes exposing (controls, href, src, style, target)
import Layout.Primitives exposing (card, sectionTitle)
import Layout.QuestionForm exposing (instructionLine, textResponse)


view : String -> (String -> msg) -> Question -> MultimodalPromptQuestion -> Html msg
view answer onAnswerChange question data =
    card
        [ sectionTitle question.title
        , p [ style "margin" "0 0 10px" ] [ text question.prompt ]
        , instructionLine question.instructions question.prompt "Observe os recursos multimodais e responda em texto."
        , p [ style "margin" "0 0 8px" ] [ text data.promptText ]
        , imageLink data.imageUrl
        , audioPlayer data.audioUrl
        , textResponse 4 (Just "Escreva sua resposta") answer onAnswerChange
        ]


imageLink : Maybe String -> Html msg
imageLink maybeUrl =
    case maybeUrl of
        Just url ->
            p [ style "margin" "0 0 8px" ]
                [ a [ href url, target "_blank" ] [ text "Abrir imagem de apoio" ] ]

        Nothing ->
            text ""


audioPlayer : Maybe String -> Html msg
audioPlayer maybeUrl =
    case maybeUrl of
        Just url ->
            audio [ controls True, style "display" "block", style "margin" "0 0 10px" ]
                [ source [ src url ] [] ]

        Nothing ->
            text ""
