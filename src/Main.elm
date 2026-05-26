module Main exposing (main)

import Browser
import Data.QuestionDecoder exposing (questionsDecoder)
import Dict exposing (Dict)
import Domain.Question exposing (Question, QuestionResponse)
import Html exposing (Html, button, div, h1, p, text)
import Html.Attributes exposing (disabled, style)
import Html.Events exposing (onClick)
import Http
import Layout.Renderer exposing (viewQuestion)


type alias Model =
    { status : Status
    , currentIndex : Int
    , responses : Dict String QuestionResponse
    }


type Status
    = Loading
    | Failed String
    | Loaded (List Question)


type Msg
    = QuestionsLoaded (Result Http.Error (List Question))
    | Next
    | Previous
    | UpdateResponse String QuestionResponse


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { status = Loading
      , currentIndex = 0
      , responses = Dict.empty
      }
    , loadQuestions
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


loadQuestions : Cmd Msg
loadQuestions =
    Http.get
        { url = "/src/Sample/questions.json"
        , expect = Http.expectJson QuestionsLoaded questionsDecoder
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        QuestionsLoaded result ->
            case result of
                Ok questions ->
                    if List.isEmpty questions then
                        ( { model | status = Failed "O arquivo JSON não possui questões." }, Cmd.none )

                    else
                        ( { model
                            | status = Loaded questions
                            , currentIndex = 0
                            , responses = Dict.empty
                          }
                        , Cmd.none
                        )

                Err err ->
                    ( { model | status = Failed (httpErrorToString err) }, Cmd.none )

        Next ->
            case model.status of
                Loaded questions ->
                    let
                        maxIndex =
                            List.length questions - 1
                    in
                    ( { model | currentIndex = clamp 0 maxIndex (model.currentIndex + 1) }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Previous ->
            ( { model | currentIndex = max 0 (model.currentIndex - 1) }, Cmd.none )

        UpdateResponse questionId response ->
            ( { model | responses = Dict.insert questionId response model.responses }, Cmd.none )


view : Model -> Html Msg
view model =
    div
        [ style "max-width" "960px"
        , style "margin" "40px auto"
        , style "padding" "0 16px 40px"
        , style "font-family" "'Noto Sans', 'Segoe UI', sans-serif"
        ]
        (headerView :: bodyView model)


headerView : Html Msg
headerView =
    div [ style "margin-bottom" "16px" ]
        [ h1 [ style "margin" "0 0 8px" ] [ text "Biblioteca de Questões - Preview" ]
        , p [ style "margin" "0", style "color" "#475569" ] [ text "Layouts reutilizáveis para ensino de língua portuguesa." ]
        ]


bodyView : Model -> List (Html Msg)
bodyView model =
    case model.status of
        Loading ->
            [ p [] [ text "Carregando questões..." ] ]

        Failed message ->
            [ p [ style "color" "#b91c1c" ] [ text message ] ]

        Loaded questions ->
            case getAt model.currentIndex questions of
                Just question ->
                    let
                        maybeResponse =
                            Dict.get question.id model.responses
                    in
                    [ p [ style "margin" "0 0 12px", style "color" "#475569" ]
                        [ text
                            ("Questão "
                                ++ String.fromInt (model.currentIndex + 1)
                                ++ " de "
                                ++ String.fromInt (List.length questions)
                            )
                        ]
                    , viewQuestion
                        maybeResponse
                        (\response -> UpdateResponse question.id response)
                        question
                    , controlsView model.currentIndex (List.length questions)
                    ]

                Nothing ->
                    [ p [ style "color" "#b91c1c" ] [ text "Índice de questão inválido." ] ]


controlsView : Int -> Int -> Html Msg
controlsView currentIndex total =
    div [ style "display" "flex", style "gap" "10px", style "margin-top" "16px" ]
        [ button
            [ onClick Previous
            , disabled (currentIndex == 0)
            , style "padding" "8px 12px"
            ]
            [ text "Anterior" ]
        , button
            [ onClick Next
            , disabled (currentIndex >= total - 1)
            , style "padding" "8px 12px"
            ]
            [ text "Próxima" ]
        ]


getAt : Int -> List a -> Maybe a
getAt index items =
    if index < 0 then
        Nothing

    else
        case ( index, items ) of
            ( _, [] ) ->
                Nothing

            ( 0, first :: _ ) ->
                Just first

            ( n, _ :: rest ) ->
                getAt (n - 1) rest


httpErrorToString : Http.Error -> String
httpErrorToString err =
    case err of
        Http.BadUrl value ->
            "URL inválida: " ++ value

        Http.Timeout ->
            "Tempo de carregamento excedido."

        Http.NetworkError ->
            "Erro de rede ao carregar o JSON local."

        Http.BadStatus statusCode ->
            "Resposta HTTP inesperada: " ++ String.fromInt statusCode

        Http.BadBody decodeError ->
            "Falha ao decodificar JSON: " ++ decodeError
