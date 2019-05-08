module View exposing (Text, text, title, toHtml, verticalRow, verticalScroll)

import Html


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


title : Text -> Block FillWidth FixedHeight msg
title (Text titleText) =
    Block { html = Html.h1 [] [ Html.text titleText ] }


text : String -> Text
text =
    Text


verticalScroll : List (Block FillWidth FixedHeight msg) -> Block FillWidth FillHeight msg
verticalScroll contents =
    Block { html = Html.div [] (contents |> List.map (\(Block { html }) -> html)) }


verticalRow : List (Block FillWidth FixedHeight msg) -> Block FillWidth FixedHeight msg
verticalRow contents =
    Block { html = Html.div [] (contents |> List.map (\(Block { html }) -> html)) }


staticCss =
    """
@import url('https://fonts.googleapis.com/css?family=Nunito');
html, body {
  width: 100%;
  height: 100%;
  margin: 0;
  padding: 0;
  overflow: hidden;
}
* {
  margin: 0;
  padding: 0;
  font-family: 'Nunito', sans-serif;
}
"""


toHtml : Block FillWidth FillHeight msg -> List (Html.Html msg)
toHtml (Block { html }) =
    [ Html.node "style" [] [ Html.text staticCss ]
    , html
    ]
