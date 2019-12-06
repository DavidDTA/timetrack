module View exposing (BothDependentBlock(..), IndependentBlock(..), WidthDependentBlock(..), toHtml)

import Html
import Html.Attributes


type IndependentBlock msg
    = PreformattedText String


type WidthDependentBlock msg
    = WrappedTextWithPerLineOverflow String
    | CenteredHorizontalWithOverflow (IndependentBlock msg)
    | VerticalList (List (WidthDependentBlock msg))


type BothDependentBlock msg
    = VerticalScroll (WidthDependentBlock msg)


staticCss =
    """
@import url('https://fonts.googleapis.com/css?family=Nunito');
html, body {
  width: 100%;
  height: 100%;
}
* {
  margin: 0;
  padding: 0;
  overflow: hidden;
  font-family: 'Nunito', sans-serif;
}
"""


toHtml : BothDependentBlock msg -> List (Html.Html msg)
toHtml block =
    [ Html.node "style" [] [ Html.text staticCss ]
    , toHtmlBothDependent block
    ]


toHtmlBothDependent : BothDependentBlock msg -> Html.Html msg
toHtmlBothDependent block =
    case block of
        VerticalScroll contents ->
            Html.div
                [ Html.Attributes.style "width" "100%"
                , Html.Attributes.style "height" "100%"
                , Html.Attributes.style "overflow-y" "scroll"
                ]
                (toHtmlWidthDependent contents)


toHtmlWidthDependent : WidthDependentBlock msg -> List (Html.Html msg)
toHtmlWidthDependent block =
    case block of
        WrappedTextWithPerLineOverflow text ->
            [ Html.div
                [ Html.Attributes.style "white-space" "pre-wrap"
                , Html.Attributes.style "text-overflow" "ellipsis"
                , Html.Attributes.style "overflow-x" "hidden"
                ]
                [ Html.text text
                ]
            ]

        CenteredHorizontalWithOverflow contents ->
            [ Html.div
                [ Html.Attributes.style "text-align" "center"
                , Html.Attributes.style "text-overflow" "ellipsis"
                , Html.Attributes.style "overflow-x" "hidden"
                ]
                (toHtmlIndependent contents)
            ]

        VerticalList contents ->
            List.concatMap toHtmlWidthDependent contents


toHtmlIndependent : IndependentBlock msg -> List (Html.Html msg)
toHtmlIndependent block =
    case block of
        PreformattedText text ->
            [ Html.span [ Html.Attributes.style "white-space" "pre" ] [ Html.text text ] ]
