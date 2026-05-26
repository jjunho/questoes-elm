module Layout.FillBlank exposing (view)

import Dict exposing (Dict)
import Domain.Question exposing (FillBlankQuestion, Question)
import Html exposing (Html, input, p, text)
import Html.Attributes exposing (placeholder, style, value)
import Html.Events exposing (onInput)
import Layout.Primitives exposing (card, instructionText, sectionTitle)


view : Dict Int String -> (Int -> String -> msg) -> Question -> FillBlankQuestion -> Html msg
view currentAnswers onInputAnswer question payload =
    card
        [ sectionTitle question.title
        , p [] [ text question.prompt ]
        , instructionLine question.instructions
        , p [] (buildSentence currentAnswers onInputAnswer payload.parts)
        , p [ style "font-size" "0.9rem", style "color" "#64748b" ] [ text ("Quantidade de lacunas: " ++ String.fromInt (List.length payload.expectedAnswers)) ]
        ]


instructionLine : Maybe String -> Html msg
instructionLine instructions =
    case instructions of
        Just value ->
            instructionText value

        Nothing ->
            instructionText "Complete as lacunas com as palavras adequadas."


buildSentence : Dict Int String -> (Int -> String -> msg) -> List String -> List (Html msg)
buildSentence currentAnswers onInputAnswer parts =
    case parts of
        [] ->
            [ text "" ]

        first :: rest ->
            text first :: interleaveBlanks currentAnswers onInputAnswer 0 rest


interleaveBlanks : Dict Int String -> (Int -> String -> msg) -> Int -> List String -> List (Html msg)
interleaveBlanks currentAnswers onInputAnswer blankIndex parts =
    case parts of
        [] ->
            []

        nextPart :: remaining ->
            [ input
                [ value (answerAt blankIndex currentAnswers)
                , placeholder ("Lacuna " ++ String.fromInt (blankIndex + 1))
                , onInput (\newValue -> onInputAnswer blankIndex newValue)
                , style "display" "inline-block"
                , style "min-width" "120px"
                , style "max-width" "220px"
                , style "border" "1px solid #1d4ed8"
                , style "border-radius" "6px"
                , style "padding" "4px 8px"
                , style "margin" "0 6px"
                ]
                []
            , text nextPart
            ]
                ++ interleaveBlanks currentAnswers onInputAnswer (blankIndex + 1) remaining


answerAt : Int -> Dict Int String -> String
answerAt index answers =
    Dict.get index answers
        |> Maybe.withDefault ""
