module Layout.JustifiedChoice exposing (view)

import Domain.Question exposing (JustifiedChoiceQuestion, Question)
import Html exposing (Html, p, text)
import Html.Attributes exposing (style)
import Layout.Primitives exposing (card, sectionTitle)
import Layout.QuestionForm exposing (choiceList, instructionLine, textResponse)


view : { selectedOption : Int, justification : String } -> (Int -> msg) -> (String -> msg) -> Question -> JustifiedChoiceQuestion -> Html msg
view answers onSelect onJustificationChange question data =
    card
        [ sectionTitle question.title
        , p [ style "margin" "0 0 10px" ] [ text question.prompt ]
        , p [ style "margin" "0 0 10px", style "padding" "10px", style "background" "#f8fafc", style "border" "1px solid #e2e8f0", style "border-radius" "8px" ] [ text data.statement ]
        , instructionLine question.instructions question.prompt "Escolha a alternativa e justifique sua resposta."
        , choiceList (selectedOption answers.selectedOption) onSelect data.options
        , justificationField data.requiresJustification answers.justification onJustificationChange
        ]


selectedOption : Int -> Maybe Int
selectedOption index =
    if index < 0 then
        Nothing

    else
        Just index


justificationField : Bool -> String -> (String -> msg) -> Html msg
justificationField isRequired currentValue onJustificationChange =
    if isRequired then
        textResponse 3 (Just "Explique sua escolha") currentValue onJustificationChange

    else
        text ""
