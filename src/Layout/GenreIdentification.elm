module Layout.GenreIdentification exposing (view)

import Domain.Question exposing (GenreIdentificationQuestion, Question)
import Html exposing (Html, div, p, text)
import Html.Attributes exposing (style)
import Layout.Primitives exposing (card, sectionTitle)
import Layout.QuestionForm exposing (choiceList, highlightBox, instructionLine)


view : Maybe Int -> (Int -> msg) -> Question -> GenreIdentificationQuestion -> Html msg
view maybeSelectedIndex onSelectOption question payload =
    card
        [ sectionTitle question.title
        , p [] [ text question.prompt ]
        , instructionLine question.instructions question.prompt "Selecione uma alternativa."
        , highlightBox payload.excerpt
        , div [ style "margin-top" "12px" ] [ choiceList maybeSelectedIndex onSelectOption payload.options ]
        ]
