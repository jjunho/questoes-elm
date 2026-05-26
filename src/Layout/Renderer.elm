module Layout.Renderer exposing (viewQuestion)

import Dict exposing (Dict)
import Domain.Question exposing (Question, QuestionPayload(..), QuestionResponse(..))
import Html exposing (Html)
import Layout.AnnotatedEdit
import Layout.Classification
import Layout.DragReorder
import Layout.ErrorCorrection
import Layout.FillBlank
import Layout.GenreIdentification
import Layout.JustifiedChoice
import Layout.Matching
import Layout.MultiSelect
import Layout.MultimodalPrompt
import Layout.MultipleChoice
import Layout.OddOneOut
import Layout.OpenQuestion
import Layout.OralInteractive
import Layout.Ordering
import Layout.SentenceTransformation
import Layout.TableInput
import Layout.Translation
import Layout.TrueFalse


viewQuestion : Maybe QuestionResponse -> (QuestionResponse -> msg) -> Question -> Html msg
viewQuestion maybeResponse onResponse question =
    case question.payload of
        MultipleChoicePayload data ->
            Layout.MultipleChoice.view
                (choiceSelection maybeResponse)
                (\index -> onResponse (ChoiceResponse index))
                question
                data

        TrueFalsePayload _ ->
            Layout.TrueFalse.view
                (selectedTrueFalse maybeResponse)
                (\value -> onResponse (TrueFalseResponse value))
                question

        FillBlankPayload payload ->
            Layout.FillBlank.view
                (fillBlankAnswers maybeResponse)
                (\blankIndex value_ -> onResponse (FillBlankResponse (Dict.insert blankIndex value_ (fillBlankAnswers maybeResponse))))
                question
                payload

        OpenQuestionPayload data ->
            Layout.OpenQuestion.view
                (textResponseValue maybeResponse)
                (\value_ -> onResponse (TextResponse value_))
                question
                data

        MatchingPayload payload ->
            Layout.Matching.view
                (matchingAnswers maybeResponse)
                (\leftIndex maybeRightIndex ->
                    let
                        current =
                            matchingAnswers maybeResponse

                        updated =
                            case maybeRightIndex of
                                Just rightIndex ->
                                    Dict.insert leftIndex rightIndex current

                                Nothing ->
                                    Dict.remove leftIndex current
                    in
                    onResponse (MatchingResponse updated)
                )
                question
                payload

        OrderingPayload payload ->
            Layout.Ordering.view
                (orderingAnswers maybeResponse)
                (\itemIndex maybePosition ->
                    let
                        current =
                            orderingAnswers maybeResponse

                        updated =
                            case maybePosition of
                                Just position ->
                                    Dict.insert itemIndex position current

                                Nothing ->
                                    Dict.remove itemIndex current
                    in
                    onResponse (OrderingResponse updated)
                )
                question
                payload

        ErrorCorrectionPayload payload ->
            Layout.ErrorCorrection.view
                (textResponseValue maybeResponse)
                (\value_ -> onResponse (TextResponse value_))
                question
                payload

        OddOneOutPayload payload ->
            Layout.OddOneOut.view
                (choiceSelection maybeResponse)
                (\index -> onResponse (ChoiceResponse index))
                question
                payload

        SentenceTransformationPayload payload ->
            Layout.SentenceTransformation.view
                (textResponseValue maybeResponse)
                (\value_ -> onResponse (TextResponse value_))
                question
                payload

        TranslationPayload payload ->
            Layout.Translation.view
                (textResponseValue maybeResponse)
                (\value_ -> onResponse (TextResponse value_))
                question
                payload

        GenreIdentificationPayload payload ->
            Layout.GenreIdentification.view
                (choiceSelection maybeResponse)
                (\index -> onResponse (ChoiceResponse index))
                question
                payload

        MultiSelectPayload payload ->
            Layout.MultiSelect.view
                (multiSelectAnswers maybeResponse)
                (\optionIndex isChecked ->
                    let
                        current =
                            multiSelectAnswers maybeResponse

                        updated =
                            Dict.insert optionIndex isChecked current
                    in
                    onResponse (MultiSelectResponse updated)
                )
                question
                payload

        ClassificationPayload payload ->
            Layout.Classification.view
                (classificationAnswers maybeResponse)
                (\itemIndex maybeLabelIndex ->
                    let
                        current =
                            classificationAnswers maybeResponse

                        updated =
                            case maybeLabelIndex of
                                Just labelIndex ->
                                    Dict.insert itemIndex labelIndex current

                                Nothing ->
                                    Dict.remove itemIndex current
                    in
                    onResponse (ClassificationResponse updated)
                )
                question
                payload

        DragReorderPayload payload ->
            Layout.DragReorder.view
                (dragReorderAnswers maybeResponse)
                (\itemIndex maybePosition ->
                    let
                        current =
                            dragReorderAnswers maybeResponse

                        updated =
                            case maybePosition of
                                Just position ->
                                    Dict.insert itemIndex position current

                                Nothing ->
                                    Dict.remove itemIndex current
                    in
                    onResponse (DragReorderResponse updated)
                )
                question
                payload

        AnnotatedEditPayload payload ->
            let
                current =
                    annotatedEditAnswers maybeResponse
            in
            Layout.AnnotatedEdit.view
                current
                (\highlightedText ->
                    onResponse
                        (AnnotatedEditResponse
                            { highlightedText = highlightedText
                            , correction = current.correction
                            }
                        )
                )
                (\correction ->
                    onResponse
                        (AnnotatedEditResponse
                            { highlightedText = current.highlightedText
                            , correction = correction
                            }
                        )
                )
                question
                payload

        TableInputPayload payload ->
            Layout.TableInput.view
                (tableInputAnswers maybeResponse)
                (\rowIndex columnIndex value_ ->
                    let
                        current =
                            tableInputAnswers maybeResponse

                        rowValues =
                            Dict.get rowIndex current
                                |> Maybe.withDefault Dict.empty

                        updated =
                            Dict.insert rowIndex (Dict.insert columnIndex value_ rowValues) current
                    in
                    onResponse (TableInputResponse updated)
                )
                question
                payload

        MultimodalPromptPayload payload ->
            Layout.MultimodalPrompt.view
                (multimodalPromptAnswer maybeResponse)
                (\value_ -> onResponse (MultimodalPromptResponse value_))
                question
                payload

        OralInteractivePayload payload ->
            let
                current =
                    oralInteractiveAnswers maybeResponse
            in
            Layout.OralInteractive.view
                current
                (\transcript ->
                    onResponse
                        (OralInteractiveResponse
                            { transcript = transcript
                            , notes = current.notes
                            }
                        )
                )
                (\notes ->
                    onResponse
                        (OralInteractiveResponse
                            { transcript = current.transcript
                            , notes = notes
                            }
                        )
                )
                question
                payload

        JustifiedChoicePayload payload ->
            let
                current =
                    justifiedChoiceAnswers maybeResponse
            in
            Layout.JustifiedChoice.view
                current
                (\selectedOption ->
                    onResponse
                        (JustifiedChoiceResponse
                            { selectedOption = selectedOption
                            , justification = current.justification
                            }
                        )
                )
                (\justification ->
                    onResponse
                        (JustifiedChoiceResponse
                            { selectedOption = current.selectedOption
                            , justification = justification
                            }
                        )
                )
                question
                payload


choiceSelection : Maybe QuestionResponse -> Maybe Int
choiceSelection maybeResponse =
    case maybeResponse of
        Just (ChoiceResponse index) ->
            Just index

        _ ->
            Nothing


selectedTrueFalse : Maybe QuestionResponse -> Maybe Bool
selectedTrueFalse maybeResponse =
    case maybeResponse of
        Just (TrueFalseResponse value_) ->
            Just value_

        _ ->
            Nothing


fillBlankAnswers : Maybe QuestionResponse -> Dict Int String
fillBlankAnswers maybeResponse =
    case maybeResponse of
        Just (FillBlankResponse answers) ->
            answers

        _ ->
            Dict.empty


textResponseValue : Maybe QuestionResponse -> String
textResponseValue maybeResponse =
    case maybeResponse of
        Just (TextResponse value_) ->
            value_

        _ ->
            ""


matchingAnswers : Maybe QuestionResponse -> Dict Int Int
matchingAnswers maybeResponse =
    case maybeResponse of
        Just (MatchingResponse answers) ->
            answers

        _ ->
            Dict.empty


orderingAnswers : Maybe QuestionResponse -> Dict Int Int
orderingAnswers maybeResponse =
    case maybeResponse of
        Just (OrderingResponse answers) ->
            answers

        _ ->
            Dict.empty


multiSelectAnswers : Maybe QuestionResponse -> Dict Int Bool
multiSelectAnswers maybeResponse =
    case maybeResponse of
        Just (MultiSelectResponse answers) ->
            answers

        _ ->
            Dict.empty


classificationAnswers : Maybe QuestionResponse -> Dict Int Int
classificationAnswers maybeResponse =
    case maybeResponse of
        Just (ClassificationResponse answers) ->
            answers

        _ ->
            Dict.empty


dragReorderAnswers : Maybe QuestionResponse -> Dict Int Int
dragReorderAnswers maybeResponse =
    case maybeResponse of
        Just (DragReorderResponse answers) ->
            answers

        _ ->
            Dict.empty


annotatedEditAnswers : Maybe QuestionResponse -> { highlightedText : String, correction : String }
annotatedEditAnswers maybeResponse =
    case maybeResponse of
        Just (AnnotatedEditResponse answers) ->
            answers

        _ ->
            { highlightedText = "", correction = "" }


tableInputAnswers : Maybe QuestionResponse -> Dict Int (Dict Int String)
tableInputAnswers maybeResponse =
    case maybeResponse of
        Just (TableInputResponse answers) ->
            answers

        _ ->
            Dict.empty


multimodalPromptAnswer : Maybe QuestionResponse -> String
multimodalPromptAnswer maybeResponse =
    case maybeResponse of
        Just (MultimodalPromptResponse value_) ->
            value_

        _ ->
            ""


oralInteractiveAnswers : Maybe QuestionResponse -> { transcript : String, notes : String }
oralInteractiveAnswers maybeResponse =
    case maybeResponse of
        Just (OralInteractiveResponse answers) ->
            answers

        _ ->
            { transcript = "", notes = "" }


justifiedChoiceAnswers : Maybe QuestionResponse -> { selectedOption : Int, justification : String }
justifiedChoiceAnswers maybeResponse =
    case maybeResponse of
        Just (JustifiedChoiceResponse answers) ->
            answers

        Just (ChoiceResponse index) ->
            { selectedOption = index, justification = "" }

        _ ->
            { selectedOption = 0, justification = "" }
