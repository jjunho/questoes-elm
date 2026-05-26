module Domain.QuestionType exposing (QuestionType(..), fromString, toString)


type QuestionType
    = MultipleChoice
    | TrueFalse
    | FillBlank
    | OpenQuestion
    | Matching
    | Ordering
    | ErrorCorrection
    | OddOneOut
    | SentenceTransformation
    | Translation
    | GenreIdentification
    | MultiSelect
    | Classification
    | DragReorder
    | AnnotatedEdit
    | TableInput
    | MultimodalPrompt
    | OralInteractive
    | JustifiedChoice


fromString : String -> Maybe QuestionType
fromString value =
    case value of
        "multiple_choice" ->
            Just MultipleChoice

        "true_false" ->
            Just TrueFalse

        "fill_blank" ->
            Just FillBlank

        "open_question" ->
            Just OpenQuestion

        "matching" ->
            Just Matching

        "ordering" ->
            Just Ordering

        "error_correction" ->
            Just ErrorCorrection

        "odd_one_out" ->
            Just OddOneOut

        "sentence_transformation" ->
            Just SentenceTransformation

        "translation" ->
            Just Translation

        "genre_identification" ->
            Just GenreIdentification

        "multi_select" ->
            Just MultiSelect

        "multiple_select" ->
            Just MultiSelect

        "classification" ->
            Just Classification

        "drag_reorder" ->
            Just DragReorder

        "reordering" ->
            Just DragReorder

        "annotated_edit" ->
            Just AnnotatedEdit

        "annotated_correction" ->
            Just AnnotatedEdit

        "table_input" ->
            Just TableInput

        "table_response" ->
            Just TableInput

        "multimodal_prompt" ->
            Just MultimodalPrompt

        "guided_response" ->
            Just MultimodalPrompt

        "oral_interactive" ->
            Just OralInteractive

        "oral_interaction" ->
            Just OralInteractive

        "justified_choice" ->
            Just JustifiedChoice

        "choice_with_justification" ->
            Just JustifiedChoice

        _ ->
            Nothing


toString : QuestionType -> String
toString questionType =
    case questionType of
        MultipleChoice ->
            "multiple_choice"

        TrueFalse ->
            "true_false"

        FillBlank ->
            "fill_blank"

        OpenQuestion ->
            "open_question"

        Matching ->
            "matching"

        Ordering ->
            "ordering"

        ErrorCorrection ->
            "error_correction"

        OddOneOut ->
            "odd_one_out"

        SentenceTransformation ->
            "sentence_transformation"

        Translation ->
            "translation"

        GenreIdentification ->
            "genre_identification"

        MultiSelect ->
            "multiple_select"

        Classification ->
            "classification"

        DragReorder ->
            "reordering"

        AnnotatedEdit ->
            "annotated_correction"

        TableInput ->
            "table_response"

        MultimodalPrompt ->
            "guided_response"

        OralInteractive ->
            "oral_interaction"

        JustifiedChoice ->
            "choice_with_justification"
