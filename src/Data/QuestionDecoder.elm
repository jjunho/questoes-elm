module Data.QuestionDecoder exposing (questionsDecoder)

import Domain.Question exposing (AnnotatedEditQuestion, ClassificationQuestion, DragReorderQuestion, ErrorCorrectionQuestion, GenreIdentificationQuestion, JustifiedChoiceQuestion, MatchingPair, MultiSelectQuestion, MultimodalPromptQuestion, MultipleChoiceQuestion, OddOneOutQuestion, OpenQuestionData, OralInteractiveQuestion, OrderingQuestion, Question, QuestionPayload(..), SentenceTransformationQuestion, TableInputQuestion, TranslationQuestion)
import Domain.QuestionType exposing (QuestionType(..), fromString)
import Json.Decode as Decode exposing (Decoder)
import String


questionsDecoder : Decoder (List Question)
questionsDecoder =
    Decode.list questionEntryDecoder
        |> Decode.map List.concat


type alias GroupCommon =
    { id : String
    , title : String
    , prompt : String
    , instructions : Maybe String
    }


type alias MultipleChoiceGroupItem =
    { id : String
    , title : Maybe String
    , prompt : Maybe String
    , instructions : Maybe String
    , options : List String
    , correctOptionIndex : Maybe Int
    }


type alias TrueFalseGroupItem =
    { id : String
    , title : Maybe String
    , prompt : Maybe String
    , instructions : Maybe String
    , answer : Maybe Bool
    }


type alias FillBlankGroupItem =
    { id : String
    , title : Maybe String
    , prompt : Maybe String
    , instructions : Maybe String
    , parts : List String
    , answers : List String
    }


type alias OpenQuestionGroupItem =
    { id : String
    , title : Maybe String
    , prompt : Maybe String
    , instructions : Maybe String
    , placeholder : Maybe String
    }


type alias MatchingGroupItem =
    { id : String
    , title : Maybe String
    , prompt : Maybe String
    , instructions : Maybe String
    , pairs : List MatchingPair
    }


type alias OrderingGroupItem =
    { id : String
    , title : Maybe String
    , prompt : Maybe String
    , instructions : Maybe String
    , items : List String
    , correctOrder : Maybe (List Int)
    }


type alias ErrorCorrectionGroupItem =
    { id : String
    , title : Maybe String
    , prompt : Maybe String
    , instructions : Maybe String
    , sentence : String
    , expectedCorrection : Maybe String
    }


type alias OddOneOutGroupItem =
    { id : String
    , title : Maybe String
    , prompt : Maybe String
    , instructions : Maybe String
    , options : List String
    , correctOptionIndex : Maybe Int
    }


type alias SentenceTransformationGroupItem =
    { id : String
    , title : Maybe String
    , prompt : Maybe String
    , instructions : Maybe String
    , sourceSentence : String
    , transformationRule : String
    , expectedAnswer : Maybe String
    }


type alias TranslationGroupItem =
    { id : String
    , title : Maybe String
    , prompt : Maybe String
    , instructions : Maybe String
    , sourceText : String
    , sourceLang : String
    , targetLang : String
    , expectedTranslation : Maybe String
    }


type alias GenreIdentificationGroupItem =
    { id : String
    , title : Maybe String
    , prompt : Maybe String
    , instructions : Maybe String
    , excerpt : String
    , options : List String
    , correctOptionIndex : Maybe Int
    }


type alias MultiSelectGroupItem =
    { id : String
    , title : Maybe String
    , prompt : Maybe String
    , instructions : Maybe String
    , options : List String
    , minSelections : Maybe Int
    , maxSelections : Maybe Int
    , correctOptionIndices : Maybe (List Int)
    }


type alias ClassificationGroupItem =
    { id : String
    , title : Maybe String
    , prompt : Maybe String
    , instructions : Maybe String
    , items : List String
    , labels : List String
    , expectedLabels : Maybe (List Int)
    }


type alias DragReorderGroupItem =
    { id : String
    , title : Maybe String
    , prompt : Maybe String
    , instructions : Maybe String
    , items : List String
    , correctOrder : Maybe (List Int)
    }


type alias AnnotatedEditGroupItem =
    { id : String
    , title : Maybe String
    , prompt : Maybe String
    , instructions : Maybe String
    , sourceText : String
    , expectedCorrection : Maybe String
    }


type alias TableInputGroupItem =
    { id : String
    , title : Maybe String
    , prompt : Maybe String
    , instructions : Maybe String
    , columns : List String
    , rowCount : Int
    , expectedRows : Maybe (List (List String))
    }


type alias MultimodalPromptGroupItem =
    { id : String
    , title : Maybe String
    , prompt : Maybe String
    , instructions : Maybe String
    , promptText : String
    , imageUrl : Maybe String
    , audioUrl : Maybe String
    , expectedResponse : Maybe String
    }


type alias OralInteractiveGroupItem =
    { id : String
    , title : Maybe String
    , prompt : Maybe String
    , instructions : Maybe String
    , scenario : String
    , role : String
    , expectedPoints : Maybe (List String)
    }


type alias JustifiedChoiceGroupItem =
    { id : String
    , title : Maybe String
    , prompt : Maybe String
    , instructions : Maybe String
    , statement : String
    , options : List String
    , requiresJustification : Bool
    , expectedOptionIndex : Maybe Int
    }


questionEntryDecoder : Decoder (List Question)
questionEntryDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen entryByTypeDecoder


entryByTypeDecoder : String -> Decoder (List Question)
entryByTypeDecoder typeValue =
    let
        normalizedType =
            normalizeType typeValue
    in
    case fromString normalizedType of
        Just questionType ->
            Decode.oneOf
                [ groupByTypeDecoder questionType
                , questionByTypeDecoder normalizedType
                    |> Decode.map List.singleton
                ]

        Nothing ->
            Decode.fail ("Unsupported question type: " ++ typeValue)


normalizeType : String -> String
normalizeType typeValue =
    if String.endsWith "_group" typeValue then
        String.dropRight 6 typeValue

    else
        typeValue


groupByTypeDecoder : QuestionType -> Decoder (List Question)
groupByTypeDecoder questionType =
    case questionType of
        MultipleChoice ->
            multipleChoiceGroupDecoder

        TrueFalse ->
            trueFalseGroupDecoder

        FillBlank ->
            fillBlankGroupDecoder

        OpenQuestion ->
            openQuestionGroupDecoder

        Matching ->
            matchingGroupDecoder

        Ordering ->
            orderingGroupDecoder

        ErrorCorrection ->
            errorCorrectionGroupDecoder

        OddOneOut ->
            oddOneOutGroupDecoder

        SentenceTransformation ->
            sentenceTransformationGroupDecoder

        Translation ->
            translationGroupDecoder

        GenreIdentification ->
            genreIdentificationGroupDecoder

        MultiSelect ->
            multiSelectGroupDecoder

        Classification ->
            classificationGroupDecoder

        DragReorder ->
            dragReorderGroupDecoder

        AnnotatedEdit ->
            annotatedEditGroupDecoder

        TableInput ->
            tableInputGroupDecoder

        MultimodalPrompt ->
            multimodalPromptGroupDecoder

        OralInteractive ->
            oralInteractiveGroupDecoder

        JustifiedChoice ->
            justifiedChoiceGroupDecoder


questionByTypeDecoder : String -> Decoder Question
questionByTypeDecoder typeValue =
    case fromString typeValue of
        Just MultipleChoice ->
            commonFieldsDecoder multipleChoicePayloadDecoder

        Just TrueFalse ->
            commonFieldsDecoder trueFalsePayloadDecoder

        Just FillBlank ->
            commonFieldsDecoder fillBlankPayloadDecoder

        Just OpenQuestion ->
            commonFieldsDecoder openQuestionPayloadDecoder

        Just Matching ->
            commonFieldsDecoder matchingPayloadDecoder

        Just Ordering ->
            commonFieldsDecoder orderingPayloadDecoder

        Just ErrorCorrection ->
            commonFieldsDecoder errorCorrectionPayloadDecoder

        Just OddOneOut ->
            commonFieldsDecoder oddOneOutPayloadDecoder

        Just SentenceTransformation ->
            commonFieldsDecoder sentenceTransformationPayloadDecoder

        Just Translation ->
            commonFieldsDecoder translationPayloadDecoder

        Just GenreIdentification ->
            commonFieldsDecoder genreIdentificationPayloadDecoder

        Just MultiSelect ->
            commonFieldsDecoder multiSelectPayloadDecoder

        Just Classification ->
            commonFieldsDecoder classificationPayloadDecoder

        Just DragReorder ->
            commonFieldsDecoder dragReorderPayloadDecoder

        Just AnnotatedEdit ->
            commonFieldsDecoder annotatedEditPayloadDecoder

        Just TableInput ->
            commonFieldsDecoder tableInputPayloadDecoder

        Just MultimodalPrompt ->
            commonFieldsDecoder multimodalPromptPayloadDecoder

        Just OralInteractive ->
            commonFieldsDecoder oralInteractivePayloadDecoder

        Just JustifiedChoice ->
            commonFieldsDecoder justifiedChoicePayloadDecoder

        Nothing ->
            Decode.fail ("Unsupported question type: " ++ typeValue)


groupCommonDecoder : Decoder GroupCommon
groupCommonDecoder =
    Decode.map4 GroupCommon
        (Decode.field "id" Decode.string)
        (Decode.field "title" Decode.string)
        (Decode.field "prompt" Decode.string)
        (Decode.maybe (Decode.field "instructions" Decode.string))


multipleChoiceGroupDecoder : Decoder (List Question)
multipleChoiceGroupDecoder =
    Decode.map2 buildMultipleChoiceGroup
        groupCommonDecoder
        (Decode.field "items" (Decode.list multipleChoiceGroupItemDecoder))


buildMultipleChoiceGroup : GroupCommon -> List MultipleChoiceGroupItem -> List Question
buildMultipleChoiceGroup common items =
    List.map
        (\item ->
            { id = item.id
            , title = Maybe.withDefault common.title item.title
            , prompt = Maybe.withDefault common.prompt item.prompt
            , instructions = firstJust item.instructions common.instructions
            , payload =
                MultipleChoicePayload
                    { options = item.options
                    , correctOptionIndex = item.correctOptionIndex
                    }
            }
        )
        items


multipleChoiceGroupItemDecoder : Decoder MultipleChoiceGroupItem
multipleChoiceGroupItemDecoder =
    Decode.map6 MultipleChoiceGroupItem
        (Decode.field "id" Decode.string)
        (Decode.maybe (Decode.field "title" Decode.string))
        (Decode.maybe (Decode.field "prompt" Decode.string))
        (Decode.maybe (Decode.field "instructions" Decode.string))
        (Decode.field "options" (Decode.list Decode.string))
        (Decode.maybe (Decode.field "correctOptionIndex" Decode.int))


trueFalseGroupDecoder : Decoder (List Question)
trueFalseGroupDecoder =
    Decode.map2 buildTrueFalseGroup
        groupCommonDecoder
        (Decode.field "items" (Decode.list trueFalseGroupItemDecoder))


buildTrueFalseGroup : GroupCommon -> List TrueFalseGroupItem -> List Question
buildTrueFalseGroup common items =
    List.map
        (\item ->
            { id = item.id
            , title = Maybe.withDefault common.title item.title
            , prompt = Maybe.withDefault common.prompt item.prompt
            , instructions = firstJust item.instructions common.instructions
            , payload =
                TrueFalsePayload
                    { expectedAnswer = item.answer }
            }
        )
        items


trueFalseGroupItemDecoder : Decoder TrueFalseGroupItem
trueFalseGroupItemDecoder =
    Decode.map5 TrueFalseGroupItem
        (Decode.field "id" Decode.string)
        (Decode.maybe (Decode.field "title" Decode.string))
        (Decode.maybe (Decode.field "prompt" Decode.string))
        (Decode.maybe (Decode.field "instructions" Decode.string))
        (Decode.maybe (Decode.field "answer" Decode.bool))


fillBlankGroupDecoder : Decoder (List Question)
fillBlankGroupDecoder =
    Decode.map2 buildFillBlankGroup
        groupCommonDecoder
        (Decode.field "items" (Decode.list fillBlankGroupItemDecoder))


buildFillBlankGroup : GroupCommon -> List FillBlankGroupItem -> List Question
buildFillBlankGroup common items =
    List.map
        (\item ->
            { id = item.id
            , title = Maybe.withDefault common.title item.title
            , prompt = Maybe.withDefault common.prompt item.prompt
            , instructions = firstJust item.instructions common.instructions
            , payload =
                FillBlankPayload
                    { parts = item.parts
                    , expectedAnswers = item.answers
                    }
            }
        )
        items


fillBlankGroupItemDecoder : Decoder FillBlankGroupItem
fillBlankGroupItemDecoder =
    Decode.map6 FillBlankGroupItem
        (Decode.field "id" Decode.string)
        (Decode.maybe (Decode.field "title" Decode.string))
        (Decode.maybe (Decode.field "prompt" Decode.string))
        (Decode.maybe (Decode.field "instructions" Decode.string))
        (Decode.field "parts" (Decode.list Decode.string))
        (Decode.field "answers" (Decode.list Decode.string))


openQuestionGroupDecoder : Decoder (List Question)
openQuestionGroupDecoder =
    Decode.map2 buildOpenQuestionGroup
        groupCommonDecoder
        (Decode.field "items" (Decode.list openQuestionGroupItemDecoder))


buildOpenQuestionGroup : GroupCommon -> List OpenQuestionGroupItem -> List Question
buildOpenQuestionGroup common items =
    List.map
        (\item ->
            { id = item.id
            , title = Maybe.withDefault common.title item.title
            , prompt = Maybe.withDefault common.prompt item.prompt
            , instructions = firstJust item.instructions common.instructions
            , payload =
                OpenQuestionPayload
                    { placeholder = Maybe.withDefault "Escreva sua resposta" item.placeholder }
            }
        )
        items


openQuestionGroupItemDecoder : Decoder OpenQuestionGroupItem
openQuestionGroupItemDecoder =
    Decode.map5 OpenQuestionGroupItem
        (Decode.field "id" Decode.string)
        (Decode.maybe (Decode.field "title" Decode.string))
        (Decode.maybe (Decode.field "prompt" Decode.string))
        (Decode.maybe (Decode.field "instructions" Decode.string))
        (Decode.maybe (Decode.field "placeholder" Decode.string))


matchingGroupDecoder : Decoder (List Question)
matchingGroupDecoder =
    Decode.map2 buildMatchingGroup
        groupCommonDecoder
        (Decode.field "items" (Decode.list matchingGroupItemDecoder))


buildMatchingGroup : GroupCommon -> List MatchingGroupItem -> List Question
buildMatchingGroup common items =
    List.map
        (\item ->
            { id = item.id
            , title = Maybe.withDefault common.title item.title
            , prompt = Maybe.withDefault common.prompt item.prompt
            , instructions = firstJust item.instructions common.instructions
            , payload =
                MatchingPayload
                    { pairs = item.pairs }
            }
        )
        items


matchingGroupItemDecoder : Decoder MatchingGroupItem
matchingGroupItemDecoder =
    Decode.map5 MatchingGroupItem
        (Decode.field "id" Decode.string)
        (Decode.maybe (Decode.field "title" Decode.string))
        (Decode.maybe (Decode.field "prompt" Decode.string))
        (Decode.maybe (Decode.field "instructions" Decode.string))
        (Decode.field "pairs" (Decode.list matchingPairDecoder))


orderingGroupDecoder : Decoder (List Question)
orderingGroupDecoder =
    Decode.map2 buildOrderingGroup
        groupCommonDecoder
        (Decode.field "items" (Decode.list orderingGroupItemDecoder))


buildOrderingGroup : GroupCommon -> List OrderingGroupItem -> List Question
buildOrderingGroup common items =
    List.map
        (\item ->
            { id = item.id
            , title = Maybe.withDefault common.title item.title
            , prompt = Maybe.withDefault common.prompt item.prompt
            , instructions = firstJust item.instructions common.instructions
            , payload =
                OrderingPayload
                    { items = item.items
                    , correctOrder = item.correctOrder
                    }
            }
        )
        items


orderingGroupItemDecoder : Decoder OrderingGroupItem
orderingGroupItemDecoder =
    Decode.map6 OrderingGroupItem
        (Decode.field "id" Decode.string)
        (Decode.maybe (Decode.field "title" Decode.string))
        (Decode.maybe (Decode.field "prompt" Decode.string))
        (Decode.maybe (Decode.field "instructions" Decode.string))
        (Decode.field "items" (Decode.list Decode.string))
        (Decode.maybe (Decode.field "correctOrder" (Decode.list Decode.int)))


errorCorrectionGroupDecoder : Decoder (List Question)
errorCorrectionGroupDecoder =
    Decode.map2 buildErrorCorrectionGroup
        groupCommonDecoder
        (Decode.field "items" (Decode.list errorCorrectionGroupItemDecoder))


buildErrorCorrectionGroup : GroupCommon -> List ErrorCorrectionGroupItem -> List Question
buildErrorCorrectionGroup common items =
    List.map
        (\item ->
            { id = item.id
            , title = Maybe.withDefault common.title item.title
            , prompt = Maybe.withDefault common.prompt item.prompt
            , instructions = firstJust item.instructions common.instructions
            , payload =
                ErrorCorrectionPayload
                    { sentence = item.sentence
                    , expectedCorrection = item.expectedCorrection
                    }
            }
        )
        items


errorCorrectionGroupItemDecoder : Decoder ErrorCorrectionGroupItem
errorCorrectionGroupItemDecoder =
    Decode.map6 ErrorCorrectionGroupItem
        (Decode.field "id" Decode.string)
        (Decode.maybe (Decode.field "title" Decode.string))
        (Decode.maybe (Decode.field "prompt" Decode.string))
        (Decode.maybe (Decode.field "instructions" Decode.string))
        (Decode.field "sentence" Decode.string)
        (Decode.maybe (Decode.field "expectedCorrection" Decode.string))


oddOneOutGroupDecoder : Decoder (List Question)
oddOneOutGroupDecoder =
    Decode.map2 buildOddOneOutGroup
        groupCommonDecoder
        (Decode.field "items" (Decode.list oddOneOutGroupItemDecoder))


buildOddOneOutGroup : GroupCommon -> List OddOneOutGroupItem -> List Question
buildOddOneOutGroup common items =
    List.map
        (\item ->
            { id = item.id
            , title = Maybe.withDefault common.title item.title
            , prompt = Maybe.withDefault common.prompt item.prompt
            , instructions = firstJust item.instructions common.instructions
            , payload =
                OddOneOutPayload
                    { options = item.options
                    , correctOptionIndex = item.correctOptionIndex
                    }
            }
        )
        items


oddOneOutGroupItemDecoder : Decoder OddOneOutGroupItem
oddOneOutGroupItemDecoder =
    Decode.map6 OddOneOutGroupItem
        (Decode.field "id" Decode.string)
        (Decode.maybe (Decode.field "title" Decode.string))
        (Decode.maybe (Decode.field "prompt" Decode.string))
        (Decode.maybe (Decode.field "instructions" Decode.string))
        (Decode.field "options" (Decode.list Decode.string))
        (Decode.maybe (Decode.field "correctOptionIndex" Decode.int))


sentenceTransformationGroupDecoder : Decoder (List Question)
sentenceTransformationGroupDecoder =
    Decode.map2 buildSentenceTransformationGroup
        groupCommonDecoder
        (Decode.field "items" (Decode.list sentenceTransformationGroupItemDecoder))


buildSentenceTransformationGroup : GroupCommon -> List SentenceTransformationGroupItem -> List Question
buildSentenceTransformationGroup common items =
    List.map
        (\item ->
            { id = item.id
            , title = Maybe.withDefault common.title item.title
            , prompt = Maybe.withDefault common.prompt item.prompt
            , instructions = firstJust item.instructions common.instructions
            , payload =
                SentenceTransformationPayload
                    { sourceSentence = item.sourceSentence
                    , transformationRule = item.transformationRule
                    , expectedAnswer = item.expectedAnswer
                    }
            }
        )
        items


sentenceTransformationGroupItemDecoder : Decoder SentenceTransformationGroupItem
sentenceTransformationGroupItemDecoder =
    Decode.map7 SentenceTransformationGroupItem
        (Decode.field "id" Decode.string)
        (Decode.maybe (Decode.field "title" Decode.string))
        (Decode.maybe (Decode.field "prompt" Decode.string))
        (Decode.maybe (Decode.field "instructions" Decode.string))
        (Decode.field "sourceSentence" Decode.string)
        (Decode.field "transformationRule" Decode.string)
        (Decode.maybe (Decode.field "expectedAnswer" Decode.string))


translationGroupDecoder : Decoder (List Question)
translationGroupDecoder =
    Decode.map2 buildTranslationGroup
        groupCommonDecoder
        (Decode.field "items" (Decode.list translationGroupItemDecoder))


buildTranslationGroup : GroupCommon -> List TranslationGroupItem -> List Question
buildTranslationGroup common items =
    List.map
        (\item ->
            { id = item.id
            , title = Maybe.withDefault common.title item.title
            , prompt = Maybe.withDefault common.prompt item.prompt
            , instructions = firstJust item.instructions common.instructions
            , payload =
                TranslationPayload
                    { sourceText = item.sourceText
                    , sourceLang = item.sourceLang
                    , targetLang = item.targetLang
                    , expectedTranslation = item.expectedTranslation
                    }
            }
        )
        items


translationGroupItemDecoder : Decoder TranslationGroupItem
translationGroupItemDecoder =
    Decode.map8 TranslationGroupItem
        (Decode.field "id" Decode.string)
        (Decode.maybe (Decode.field "title" Decode.string))
        (Decode.maybe (Decode.field "prompt" Decode.string))
        (Decode.maybe (Decode.field "instructions" Decode.string))
        (Decode.field "sourceText" Decode.string)
        (Decode.field "sourceLang" Decode.string)
        (Decode.field "targetLang" Decode.string)
        (Decode.maybe (Decode.field "expectedTranslation" Decode.string))


genreIdentificationGroupDecoder : Decoder (List Question)
genreIdentificationGroupDecoder =
    Decode.map2 buildGenreIdentificationGroup
        groupCommonDecoder
        (Decode.field "items" (Decode.list genreIdentificationGroupItemDecoder))


buildGenreIdentificationGroup : GroupCommon -> List GenreIdentificationGroupItem -> List Question
buildGenreIdentificationGroup common items =
    List.map
        (\item ->
            { id = item.id
            , title = Maybe.withDefault common.title item.title
            , prompt = Maybe.withDefault common.prompt item.prompt
            , instructions = firstJust item.instructions common.instructions
            , payload =
                GenreIdentificationPayload
                    { excerpt = item.excerpt
                    , options = item.options
                    , correctOptionIndex = item.correctOptionIndex
                    }
            }
        )
        items


genreIdentificationGroupItemDecoder : Decoder GenreIdentificationGroupItem
genreIdentificationGroupItemDecoder =
    Decode.map7 GenreIdentificationGroupItem
        (Decode.field "id" Decode.string)
        (Decode.maybe (Decode.field "title" Decode.string))
        (Decode.maybe (Decode.field "prompt" Decode.string))
        (Decode.maybe (Decode.field "instructions" Decode.string))
        (Decode.field "excerpt" Decode.string)
        (Decode.field "options" (Decode.list Decode.string))
        (Decode.maybe (Decode.field "correctOptionIndex" Decode.int))


multiSelectGroupDecoder : Decoder (List Question)
multiSelectGroupDecoder =
    Decode.map2 buildMultiSelectGroup
        groupCommonDecoder
        (Decode.field "items" (Decode.list multiSelectGroupItemDecoder))


buildMultiSelectGroup : GroupCommon -> List MultiSelectGroupItem -> List Question
buildMultiSelectGroup common items =
    List.map
        (\item ->
            { id = item.id
            , title = Maybe.withDefault common.title item.title
            , prompt = Maybe.withDefault common.prompt item.prompt
            , instructions = firstJust item.instructions common.instructions
            , payload =
                MultiSelectPayload
                    { options = item.options
                    , minSelections = item.minSelections
                    , maxSelections = item.maxSelections
                    , correctOptionIndices = item.correctOptionIndices
                    }
            }
        )
        items


multiSelectGroupItemDecoder : Decoder MultiSelectGroupItem
multiSelectGroupItemDecoder =
    Decode.map8 MultiSelectGroupItem
        (Decode.field "id" Decode.string)
        (Decode.maybe (Decode.field "title" Decode.string))
        (Decode.maybe (Decode.field "prompt" Decode.string))
        (Decode.maybe (Decode.field "instructions" Decode.string))
        (Decode.field "options" (Decode.list Decode.string))
        (Decode.maybe (Decode.field "minSelections" Decode.int))
        (Decode.maybe (Decode.field "maxSelections" Decode.int))
        (Decode.maybe (Decode.field "correctOptionIndices" (Decode.list Decode.int)))


classificationGroupDecoder : Decoder (List Question)
classificationGroupDecoder =
    Decode.map2 buildClassificationGroup
        groupCommonDecoder
        (Decode.field "items" (Decode.list classificationGroupItemDecoder))


buildClassificationGroup : GroupCommon -> List ClassificationGroupItem -> List Question
buildClassificationGroup common items =
    List.map
        (\item ->
            { id = item.id
            , title = Maybe.withDefault common.title item.title
            , prompt = Maybe.withDefault common.prompt item.prompt
            , instructions = firstJust item.instructions common.instructions
            , payload =
                ClassificationPayload
                    { items = item.items
                    , labels = item.labels
                    , expectedLabels = item.expectedLabels
                    }
            }
        )
        items


classificationGroupItemDecoder : Decoder ClassificationGroupItem
classificationGroupItemDecoder =
    Decode.map7 ClassificationGroupItem
        (Decode.field "id" Decode.string)
        (Decode.maybe (Decode.field "title" Decode.string))
        (Decode.maybe (Decode.field "prompt" Decode.string))
        (Decode.maybe (Decode.field "instructions" Decode.string))
        (Decode.field "items" (Decode.list Decode.string))
        (Decode.field "labels" (Decode.list Decode.string))
        (Decode.maybe (Decode.field "expectedLabels" (Decode.list Decode.int)))


dragReorderGroupDecoder : Decoder (List Question)
dragReorderGroupDecoder =
    Decode.map2 buildDragReorderGroup
        groupCommonDecoder
        (Decode.field "items" (Decode.list dragReorderGroupItemDecoder))


buildDragReorderGroup : GroupCommon -> List DragReorderGroupItem -> List Question
buildDragReorderGroup common items =
    List.map
        (\item ->
            { id = item.id
            , title = Maybe.withDefault common.title item.title
            , prompt = Maybe.withDefault common.prompt item.prompt
            , instructions = firstJust item.instructions common.instructions
            , payload =
                DragReorderPayload
                    { items = item.items
                    , correctOrder = item.correctOrder
                    }
            }
        )
        items


dragReorderGroupItemDecoder : Decoder DragReorderGroupItem
dragReorderGroupItemDecoder =
    Decode.map6 DragReorderGroupItem
        (Decode.field "id" Decode.string)
        (Decode.maybe (Decode.field "title" Decode.string))
        (Decode.maybe (Decode.field "prompt" Decode.string))
        (Decode.maybe (Decode.field "instructions" Decode.string))
        (Decode.field "items" (Decode.list Decode.string))
        (Decode.maybe (Decode.field "correctOrder" (Decode.list Decode.int)))


annotatedEditGroupDecoder : Decoder (List Question)
annotatedEditGroupDecoder =
    Decode.map2 buildAnnotatedEditGroup
        groupCommonDecoder
        (Decode.field "items" (Decode.list annotatedEditGroupItemDecoder))


buildAnnotatedEditGroup : GroupCommon -> List AnnotatedEditGroupItem -> List Question
buildAnnotatedEditGroup common items =
    List.map
        (\item ->
            { id = item.id
            , title = Maybe.withDefault common.title item.title
            , prompt = Maybe.withDefault common.prompt item.prompt
            , instructions = firstJust item.instructions common.instructions
            , payload =
                AnnotatedEditPayload
                    { sourceText = item.sourceText
                    , expectedCorrection = item.expectedCorrection
                    }
            }
        )
        items


annotatedEditGroupItemDecoder : Decoder AnnotatedEditGroupItem
annotatedEditGroupItemDecoder =
    Decode.map6 AnnotatedEditGroupItem
        (Decode.field "id" Decode.string)
        (Decode.maybe (Decode.field "title" Decode.string))
        (Decode.maybe (Decode.field "prompt" Decode.string))
        (Decode.maybe (Decode.field "instructions" Decode.string))
        (Decode.field "sourceText" Decode.string)
        (Decode.maybe (Decode.field "expectedCorrection" Decode.string))


tableInputGroupDecoder : Decoder (List Question)
tableInputGroupDecoder =
    Decode.map2 buildTableInputGroup
        groupCommonDecoder
        (Decode.field "items" (Decode.list tableInputGroupItemDecoder))


buildTableInputGroup : GroupCommon -> List TableInputGroupItem -> List Question
buildTableInputGroup common items =
    List.map
        (\item ->
            { id = item.id
            , title = Maybe.withDefault common.title item.title
            , prompt = Maybe.withDefault common.prompt item.prompt
            , instructions = firstJust item.instructions common.instructions
            , payload =
                TableInputPayload
                    { columns = item.columns
                    , rowCount = item.rowCount
                    , expectedRows = item.expectedRows
                    }
            }
        )
        items


tableInputGroupItemDecoder : Decoder TableInputGroupItem
tableInputGroupItemDecoder =
    Decode.map7 TableInputGroupItem
        (Decode.field "id" Decode.string)
        (Decode.maybe (Decode.field "title" Decode.string))
        (Decode.maybe (Decode.field "prompt" Decode.string))
        (Decode.maybe (Decode.field "instructions" Decode.string))
        (Decode.field "columns" (Decode.list Decode.string))
        (Decode.field "rowCount" Decode.int)
        (Decode.maybe (Decode.field "expectedRows" (Decode.list (Decode.list Decode.string))))


multimodalPromptGroupDecoder : Decoder (List Question)
multimodalPromptGroupDecoder =
    Decode.map2 buildMultimodalPromptGroup
        groupCommonDecoder
        (Decode.field "items" (Decode.list multimodalPromptGroupItemDecoder))


buildMultimodalPromptGroup : GroupCommon -> List MultimodalPromptGroupItem -> List Question
buildMultimodalPromptGroup common items =
    List.map
        (\item ->
            { id = item.id
            , title = Maybe.withDefault common.title item.title
            , prompt = Maybe.withDefault common.prompt item.prompt
            , instructions = firstJust item.instructions common.instructions
            , payload =
                MultimodalPromptPayload
                    { promptText = item.promptText
                    , imageUrl = item.imageUrl
                    , audioUrl = item.audioUrl
                    , expectedResponse = item.expectedResponse
                    }
            }
        )
        items


multimodalPromptGroupItemDecoder : Decoder MultimodalPromptGroupItem
multimodalPromptGroupItemDecoder =
    Decode.map8 MultimodalPromptGroupItem
        (Decode.field "id" Decode.string)
        (Decode.maybe (Decode.field "title" Decode.string))
        (Decode.maybe (Decode.field "prompt" Decode.string))
        (Decode.maybe (Decode.field "instructions" Decode.string))
        (Decode.field "promptText" Decode.string)
        (Decode.maybe (Decode.field "imageUrl" Decode.string))
        (Decode.maybe (Decode.field "audioUrl" Decode.string))
        (Decode.maybe (Decode.field "expectedResponse" Decode.string))


oralInteractiveGroupDecoder : Decoder (List Question)
oralInteractiveGroupDecoder =
    Decode.map2 buildOralInteractiveGroup
        groupCommonDecoder
        (Decode.field "items" (Decode.list oralInteractiveGroupItemDecoder))


buildOralInteractiveGroup : GroupCommon -> List OralInteractiveGroupItem -> List Question
buildOralInteractiveGroup common items =
    List.map
        (\item ->
            { id = item.id
            , title = Maybe.withDefault common.title item.title
            , prompt = Maybe.withDefault common.prompt item.prompt
            , instructions = firstJust item.instructions common.instructions
            , payload =
                OralInteractivePayload
                    { scenario = item.scenario
                    , role = item.role
                    , expectedPoints = item.expectedPoints
                    }
            }
        )
        items


oralInteractiveGroupItemDecoder : Decoder OralInteractiveGroupItem
oralInteractiveGroupItemDecoder =
    Decode.map7 OralInteractiveGroupItem
        (Decode.field "id" Decode.string)
        (Decode.maybe (Decode.field "title" Decode.string))
        (Decode.maybe (Decode.field "prompt" Decode.string))
        (Decode.maybe (Decode.field "instructions" Decode.string))
        (Decode.field "scenario" Decode.string)
        (Decode.field "role" Decode.string)
        (Decode.maybe (Decode.field "expectedPoints" (Decode.list Decode.string)))


justifiedChoiceGroupDecoder : Decoder (List Question)
justifiedChoiceGroupDecoder =
    Decode.map2 buildJustifiedChoiceGroup
        groupCommonDecoder
        (Decode.field "items" (Decode.list justifiedChoiceGroupItemDecoder))


buildJustifiedChoiceGroup : GroupCommon -> List JustifiedChoiceGroupItem -> List Question
buildJustifiedChoiceGroup common items =
    List.map
        (\item ->
            { id = item.id
            , title = Maybe.withDefault common.title item.title
            , prompt = Maybe.withDefault common.prompt item.prompt
            , instructions = firstJust item.instructions common.instructions
            , payload =
                JustifiedChoicePayload
                    { statement = item.statement
                    , options = item.options
                    , requiresJustification = item.requiresJustification
                    , expectedOptionIndex = item.expectedOptionIndex
                    }
            }
        )
        items


justifiedChoiceGroupItemDecoder : Decoder JustifiedChoiceGroupItem
justifiedChoiceGroupItemDecoder =
    Decode.map8 JustifiedChoiceGroupItem
        (Decode.field "id" Decode.string)
        (Decode.maybe (Decode.field "title" Decode.string))
        (Decode.maybe (Decode.field "prompt" Decode.string))
        (Decode.maybe (Decode.field "instructions" Decode.string))
        (Decode.field "statement" Decode.string)
        (Decode.field "options" (Decode.list Decode.string))
        (Decode.oneOf
            [ Decode.field "requiresJustification" Decode.bool
            , Decode.succeed True
            ]
        )
        (Decode.maybe (Decode.field "expectedOptionIndex" Decode.int))


firstJust : Maybe a -> Maybe a -> Maybe a
firstJust first second =
    case first of
        Just value_ ->
            Just value_

        Nothing ->
            second


commonFieldsDecoder : Decoder QuestionPayload -> Decoder Question
commonFieldsDecoder payloadDecoder =
    Decode.map5
        (\id title prompt instructions payload ->
            { id = id
            , title = title
            , prompt = prompt
            , instructions = instructions
            , payload = payload
            }
        )
        (Decode.field "id" Decode.string)
        (Decode.field "title" Decode.string)
        (Decode.field "prompt" Decode.string)
        (Decode.maybe (Decode.field "instructions" Decode.string))
        payloadDecoder


multipleChoicePayloadDecoder : Decoder QuestionPayload
multipleChoicePayloadDecoder =
    Decode.map
        (\data -> MultipleChoicePayload data)
        multipleChoiceDataDecoder


multipleChoiceDataDecoder : Decoder MultipleChoiceQuestion
multipleChoiceDataDecoder =
    Decode.map2
        (\options correctOptionIndex ->
            { options = options
            , correctOptionIndex = correctOptionIndex
            }
        )
        (Decode.field "options" (Decode.list Decode.string))
        (Decode.maybe (Decode.field "correctOptionIndex" Decode.int))


trueFalsePayloadDecoder : Decoder QuestionPayload
trueFalsePayloadDecoder =
    Decode.map
        (\expectedAnswer ->
            TrueFalsePayload
                { expectedAnswer = expectedAnswer }
        )
        (Decode.maybe (Decode.field "answer" Decode.bool))


fillBlankPayloadDecoder : Decoder QuestionPayload
fillBlankPayloadDecoder =
    Decode.map2
        (\parts answers ->
            FillBlankPayload
                { parts = parts
                , expectedAnswers = answers
                }
        )
        (Decode.field "parts" (Decode.list Decode.string))
        (Decode.field "answers" (Decode.list Decode.string))


openQuestionPayloadDecoder : Decoder QuestionPayload
openQuestionPayloadDecoder =
    Decode.map
        (\data -> OpenQuestionPayload data)
        openQuestionDataDecoder


openQuestionDataDecoder : Decoder OpenQuestionData
openQuestionDataDecoder =
    Decode.map OpenQuestionData
        (Decode.oneOf
            [ Decode.field "placeholder" Decode.string
            , Decode.succeed "Escreva sua resposta"
            ]
        )


matchingPayloadDecoder : Decoder QuestionPayload
matchingPayloadDecoder =
    Decode.map
        (\pairs ->
            MatchingPayload
                { pairs = pairs }
        )
        (Decode.field "pairs" (Decode.list matchingPairDecoder))


orderingPayloadDecoder : Decoder QuestionPayload
orderingPayloadDecoder =
    Decode.map
        (\data -> OrderingPayload data)
        orderingDataDecoder


orderingDataDecoder : Decoder OrderingQuestion
orderingDataDecoder =
    Decode.map2 OrderingQuestion
        (Decode.field "items" (Decode.list Decode.string))
        (Decode.maybe (Decode.field "correctOrder" (Decode.list Decode.int)))


errorCorrectionPayloadDecoder : Decoder QuestionPayload
errorCorrectionPayloadDecoder =
    Decode.map
        (\data -> ErrorCorrectionPayload data)
        errorCorrectionDataDecoder


errorCorrectionDataDecoder : Decoder ErrorCorrectionQuestion
errorCorrectionDataDecoder =
    Decode.map2 ErrorCorrectionQuestion
        (Decode.field "sentence" Decode.string)
        (Decode.maybe (Decode.field "expectedCorrection" Decode.string))


oddOneOutPayloadDecoder : Decoder QuestionPayload
oddOneOutPayloadDecoder =
    Decode.map
        (\data -> OddOneOutPayload data)
        oddOneOutDataDecoder


oddOneOutDataDecoder : Decoder OddOneOutQuestion
oddOneOutDataDecoder =
    Decode.map2
        (\options correctOptionIndex ->
            { options = options
            , correctOptionIndex = correctOptionIndex
            }
        )
        (Decode.field "options" (Decode.list Decode.string))
        (Decode.maybe (Decode.field "correctOptionIndex" Decode.int))


sentenceTransformationPayloadDecoder : Decoder QuestionPayload
sentenceTransformationPayloadDecoder =
    Decode.map
        (\data -> SentenceTransformationPayload data)
        sentenceTransformationDataDecoder


sentenceTransformationDataDecoder : Decoder SentenceTransformationQuestion
sentenceTransformationDataDecoder =
    Decode.map3 SentenceTransformationQuestion
        (Decode.field "sourceSentence" Decode.string)
        (Decode.field "transformationRule" Decode.string)
        (Decode.maybe (Decode.field "expectedAnswer" Decode.string))


translationPayloadDecoder : Decoder QuestionPayload
translationPayloadDecoder =
    Decode.map
        (\data -> TranslationPayload data)
        translationDataDecoder


translationDataDecoder : Decoder TranslationQuestion
translationDataDecoder =
    Decode.map4 TranslationQuestion
        (Decode.field "sourceText" Decode.string)
        (Decode.field "sourceLang" Decode.string)
        (Decode.field "targetLang" Decode.string)
        (Decode.maybe (Decode.field "expectedTranslation" Decode.string))


genreIdentificationPayloadDecoder : Decoder QuestionPayload
genreIdentificationPayloadDecoder =
    Decode.map
        (\data -> GenreIdentificationPayload data)
        genreIdentificationDataDecoder


genreIdentificationDataDecoder : Decoder GenreIdentificationQuestion
genreIdentificationDataDecoder =
    Decode.map3 GenreIdentificationQuestion
        (Decode.field "excerpt" Decode.string)
        (Decode.field "options" (Decode.list Decode.string))
        (Decode.maybe (Decode.field "correctOptionIndex" Decode.int))


multiSelectPayloadDecoder : Decoder QuestionPayload
multiSelectPayloadDecoder =
    Decode.map
        (\data -> MultiSelectPayload data)
        multiSelectDataDecoder


multiSelectDataDecoder : Decoder MultiSelectQuestion
multiSelectDataDecoder =
    Decode.map4 MultiSelectQuestion
        (Decode.field "options" (Decode.list Decode.string))
        (Decode.maybe (Decode.field "minSelections" Decode.int))
        (Decode.maybe (Decode.field "maxSelections" Decode.int))
        (Decode.maybe (Decode.field "correctOptionIndices" (Decode.list Decode.int)))


classificationPayloadDecoder : Decoder QuestionPayload
classificationPayloadDecoder =
    Decode.map
        (\data -> ClassificationPayload data)
        classificationDataDecoder


classificationDataDecoder : Decoder ClassificationQuestion
classificationDataDecoder =
    Decode.map3 ClassificationQuestion
        (Decode.field "items" (Decode.list Decode.string))
        (Decode.field "labels" (Decode.list Decode.string))
        (Decode.maybe (Decode.field "expectedLabels" (Decode.list Decode.int)))


dragReorderPayloadDecoder : Decoder QuestionPayload
dragReorderPayloadDecoder =
    Decode.map
        (\data -> DragReorderPayload data)
        dragReorderDataDecoder


dragReorderDataDecoder : Decoder DragReorderQuestion
dragReorderDataDecoder =
    Decode.map2 DragReorderQuestion
        (Decode.field "items" (Decode.list Decode.string))
        (Decode.maybe (Decode.field "correctOrder" (Decode.list Decode.int)))


annotatedEditPayloadDecoder : Decoder QuestionPayload
annotatedEditPayloadDecoder =
    Decode.map
        (\data -> AnnotatedEditPayload data)
        annotatedEditDataDecoder


annotatedEditDataDecoder : Decoder AnnotatedEditQuestion
annotatedEditDataDecoder =
    Decode.map2 AnnotatedEditQuestion
        (Decode.field "sourceText" Decode.string)
        (Decode.maybe (Decode.field "expectedCorrection" Decode.string))


tableInputPayloadDecoder : Decoder QuestionPayload
tableInputPayloadDecoder =
    Decode.map
        (\data -> TableInputPayload data)
        tableInputDataDecoder


tableInputDataDecoder : Decoder TableInputQuestion
tableInputDataDecoder =
    Decode.map3 TableInputQuestion
        (Decode.field "columns" (Decode.list Decode.string))
        (Decode.field "rowCount" Decode.int)
        (Decode.maybe (Decode.field "expectedRows" (Decode.list (Decode.list Decode.string))))


multimodalPromptPayloadDecoder : Decoder QuestionPayload
multimodalPromptPayloadDecoder =
    Decode.map
        (\data -> MultimodalPromptPayload data)
        multimodalPromptDataDecoder


multimodalPromptDataDecoder : Decoder MultimodalPromptQuestion
multimodalPromptDataDecoder =
    Decode.map4 MultimodalPromptQuestion
        (Decode.field "promptText" Decode.string)
        (Decode.maybe (Decode.field "imageUrl" Decode.string))
        (Decode.maybe (Decode.field "audioUrl" Decode.string))
        (Decode.maybe (Decode.field "expectedResponse" Decode.string))


oralInteractivePayloadDecoder : Decoder QuestionPayload
oralInteractivePayloadDecoder =
    Decode.map
        (\data -> OralInteractivePayload data)
        oralInteractiveDataDecoder


oralInteractiveDataDecoder : Decoder OralInteractiveQuestion
oralInteractiveDataDecoder =
    Decode.map3 OralInteractiveQuestion
        (Decode.field "scenario" Decode.string)
        (Decode.field "role" Decode.string)
        (Decode.maybe (Decode.field "expectedPoints" (Decode.list Decode.string)))


justifiedChoicePayloadDecoder : Decoder QuestionPayload
justifiedChoicePayloadDecoder =
    Decode.map
        (\data -> JustifiedChoicePayload data)
        justifiedChoiceDataDecoder


justifiedChoiceDataDecoder : Decoder JustifiedChoiceQuestion
justifiedChoiceDataDecoder =
    Decode.map4 JustifiedChoiceQuestion
        (Decode.field "statement" Decode.string)
        (Decode.field "options" (Decode.list Decode.string))
        (Decode.oneOf
            [ Decode.field "requiresJustification" Decode.bool
            , Decode.succeed True
            ]
        )
        (Decode.maybe (Decode.field "expectedOptionIndex" Decode.int))


matchingPairDecoder : Decoder MatchingPair
matchingPairDecoder =
    Decode.map2 MatchingPair
        (Decode.field "left" Decode.string)
        (Decode.field "right" Decode.string)
