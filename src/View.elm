module View exposing (Text, text, title, toHtml, verticalRow, verticalScroll)

import Html


type Text
    = Text String


type Block height width msg
    = Block { html : Html.Html msg }


type FillHeight
    = FillHeight


type FixedHeight
    = FixedHeight


type FillWidth
    = FillWidth


type FixedWidth
    = FixedWidth


title : Text -> Block FixedHeight FillWidth msg
title (Text titleText) =
    Block { html = Html.h1 [] [ Html.text titleText ] }


text : String -> Text
text =
    Text


verticalScroll : List (Block FixedHeight FillWidth msg) -> Block FillHeight FillWidth msg
verticalScroll contents =
    Block { html = Html.div [] (contents |> List.map (\(Block { html }) -> html)) }


verticalRow : List (Block FixedHeight FillWidth msg) -> Block FixedHeight FillWidth msg
verticalRow contents =
    Block { html = Html.div [] (contents |> List.map (\(Block { html }) -> html)) }


staticCss =
    """
html, body {
  width: 100%;
  height: 100%;
  margin: 0;
  padding: 0;
  overflow: hidden;
}
"""


toHtml : Block FillHeight FillWidth msg -> List (Html.Html msg)
toHtml (Block { html }) =
    [ Html.node "style" [] [ Html.text staticCss ]
    , html
    ]
