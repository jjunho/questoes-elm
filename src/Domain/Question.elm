module Domain.Question exposing (AnnotatedEditQuestion, ChoiceQuestion, ClassificationQuestion, DragReorderQuestion, ErrorCorrectionQuestion, FillBlankQuestion, GenreIdentificationQuestion, JustifiedChoiceQuestion, MatchingPair, MatchingQuestion, MultiSelectQuestion, MultimodalPromptQuestion, MultipleChoiceQuestion, OddOneOutQuestion, OpenQuestionData, OralInteractiveQuestion, OrderingQuestion, Question, QuestionPayload(..), QuestionResponse(..), SentenceTransformationQuestion, TableInputQuestion, TranslationQuestion, TrueFalseQuestion)

import Dict exposing (Dict)


type alias ChoiceQuestion =
    { options : List String
    , correctOptionIndex : Maybe Int
    }


type alias MultipleChoiceQuestion =
    ChoiceQuestion


type alias TrueFalseQuestion =
    { expectedAnswer : Maybe Bool
    }


type alias FillBlankQuestion =
    { parts : List String
    , expectedAnswers : List String
    }


type alias MatchingPair =
    { left : String
    , right : String
    }


type alias OpenQuestionData =
    { placeholder : String
    }


type alias MatchingQuestion =
    { pairs : List MatchingPair
    }


type alias OrderingQuestion =
    { items : List String
    , correctOrder : Maybe (List Int)
    }


type alias ErrorCorrectionQuestion =
    { sentence : String
    , expectedCorrection : Maybe String
    }


type alias OddOneOutQuestion =
    ChoiceQuestion


type alias SentenceTransformationQuestion =
    { sourceSentence : String
    , transformationRule : String
    , expectedAnswer : Maybe String
    }


type alias TranslationQuestion =
    { sourceText : String
    , sourceLang : String
    , targetLang : String
    , expectedTranslation : Maybe String
    }


type alias GenreIdentificationQuestion =
    { excerpt : String
    , options : List String
    , correctOptionIndex : Maybe Int
    }


type alias MultiSelectQuestion =
    { options : List String
    , minSelections : Maybe Int
    , maxSelections : Maybe Int
    , correctOptionIndices : Maybe (List Int)
    }


type alias ClassificationQuestion =
    { items : List String
    , labels : List String
    , expectedLabels : Maybe (List Int)
    }


type alias DragReorderQuestion =
    { items : List String
    , correctOrder : Maybe (List Int)
    }


type alias AnnotatedEditQuestion =
    { sourceText : String
    , expectedCorrection : Maybe String
    }


type alias TableInputQuestion =
    { columns : List String
    , rowCount : Int
    , expectedRows : Maybe (List (List String))
    }


type alias MultimodalPromptQuestion =
    { promptText : String
    , imageUrl : Maybe String
    , audioUrl : Maybe String
    , expectedResponse : Maybe String
    }


type alias OralInteractiveQuestion =
    { scenario : String
    , role : String
    , expectedPoints : Maybe (List String)
    }


type alias JustifiedChoiceQuestion =
    { statement : String
    , options : List String
    , requiresJustification : Bool
    , expectedOptionIndex : Maybe Int
    }


type QuestionPayload
    = MultipleChoicePayload MultipleChoiceQuestion
    | TrueFalsePayload TrueFalseQuestion
    | FillBlankPayload FillBlankQuestion
    | OpenQuestionPayload OpenQuestionData
    | MatchingPayload MatchingQuestion
    | OrderingPayload OrderingQuestion
    | ErrorCorrectionPayload ErrorCorrectionQuestion
    | OddOneOutPayload OddOneOutQuestion
    | SentenceTransformationPayload SentenceTransformationQuestion
    | TranslationPayload TranslationQuestion
    | GenreIdentificationPayload GenreIdentificationQuestion
    | MultiSelectPayload MultiSelectQuestion
    | ClassificationPayload ClassificationQuestion
    | DragReorderPayload DragReorderQuestion
    | AnnotatedEditPayload AnnotatedEditQuestion
    | TableInputPayload TableInputQuestion
    | MultimodalPromptPayload MultimodalPromptQuestion
    | OralInteractivePayload OralInteractiveQuestion
    | JustifiedChoicePayload JustifiedChoiceQuestion


type QuestionResponse
    = ChoiceResponse Int
    | TrueFalseResponse Bool
    | FillBlankResponse (Dict Int String)
    | TextResponse String
    | MatchingResponse (Dict Int Int)
    | OrderingResponse (Dict Int Int)
    | MultiSelectResponse (Dict Int Bool)
    | ClassificationResponse (Dict Int Int)
    | DragReorderResponse (Dict Int Int)
    | AnnotatedEditResponse { highlightedText : String, correction : String }
    | TableInputResponse (Dict Int (Dict Int String))
    | MultimodalPromptResponse String
    | OralInteractiveResponse { transcript : String, notes : String }
    | JustifiedChoiceResponse { selectedOption : Int, justification : String }


type alias Question =
    { id : String
    , title : String
    , prompt : String
    , instructions : Maybe String
    , payload : QuestionPayload
    }
