module View exposing (Text, centered, text, toHtml, verticalList, verticalScroll)

import Html
import Html.Attributes


type Text
    = Text String


type Block width height msg
    = Block { html : Html.Html msg }


type FillHeight
    = FillHeight


type FixedHeight
    = FixedHeight


type FillWidth
    = FillWidth


type FixedWidth
    = FixedWidth


centered : Block FixedWidth FixedHeight msg -> Block FillWidth FixedHeight msg
centered (Block { html }) =
    Block { html = Html.div [ Html.Attributes.style "text-align" "center" ] [ html ] }


text : String -> Block FixedWidth FixedHeight msg
text value =
    Block { html = Html.span [ Html.Attributes.style "white-space" "pre" ] [ Html.text value ] }


verticalScroll : List (Block FillWidth FixedHeight msg) -> Block FillWidth FillHeight msg
verticalScroll contents =
    Block
        { html =
            Html.div
                [ Html.Attributes.style "overflow-y" "scroll"
                , Html.Attributes.style "height" "100%"
                ]
                (contents |> List.map (\(Block { html }) -> html))
        }


verticalList : List (Block FillWidth FixedHeight msg) -> Block FillWidth FixedHeight msg
verticalList contents =
    Block { html = Html.div [] (contents |> List.map (\(Block { html }) -> html)) }


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


toHtml : Block FillWidth FillHeight msg -> List (Html.Html msg)
toHtml (Block { html }) =
    [ Html.node "style" [] [ Html.text staticCss ]
    , html
    ]
