module Layout.Primitives exposing (card, instructionText, sectionTitle)

import Html exposing (Html, div, h2, p, text)
import Html.Attributes exposing (style)


card : List (Html msg) -> Html msg
card children =
    div
        [ style "background" "#ffffff"
        , style "border" "1px solid #d6e3ff"
        , style "border-radius" "12px"
        , style "box-shadow" "0 8px 24px rgba(0,0,0,0.06)"
        , style "padding" "20px"
        ]
        children


sectionTitle : String -> Html msg
sectionTitle value =
    h2
        [ style "font-size" "1.25rem"
        , style "margin" "0 0 10px"
        ]
        [ text value ]


instructionText : String -> Html msg
instructionText value =
    p
        [ style "margin" "0 0 12px"
        , style "color" "#334155"
        ]
        [ text value ]
