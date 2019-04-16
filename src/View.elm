module View exposing (Text, text, title, toHtml, verticalRow, verticalScroll)

import Css
import Css.Global
import Html
import Html.Styled


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


toHtml : Block FillHeight FillWidth msg -> List (Html.Html msg)
toHtml (Block { html }) =
    [ Css.Global.global
        [ Css.Global.html
            [ Css.width (Css.pct 100)
            , Css.height (Css.pct 100)
            , Css.margin Css.zero
            , Css.padding Css.zero
            , Css.overflow Css.hidden
            ]
        , Css.Global.body
            [ Css.width (Css.pct 100)
            , Css.height (Css.pct 100)
            , Css.margin Css.zero
            , Css.padding Css.zero
            , Css.overflow Css.hidden
            ]
        ]
        |> Html.Styled.toUnstyled
    , html
    ]
