----------------------------------------------------------------------
--
-- PopupPicker.elm
-- A popup <div> to pick from a list of choices.
-- Copyright (c) 2020 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE
--
----------------------------------------------------------------------


module PopupPicker exposing
    ( PopupPicker
    , makePopupPicker
    , view
    , position, top, left, bottom, right
    , zIndex
    )

{-| A popup <div> to pick from a list of choices.

See <https://developer.mozilla.org/en-US/docs/Web/CSS/position> for a
description of the relevant CSS.


# Class

@docs PopupPicker


# Initialization

@docs makePopupPicker


# Rendering

@docs view


# Generating `positionAttributes`

@docs position, top, left, bottom, right


# Ensuring the popup is on top

@docs zIndex

-}

import Html exposing (Attribute, Html, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)


{-| State passed to `view`.

`divAttributes` are added to the popup attributes for the whole `div`.

`positionAttributes` are additional attributes for the whole `div`,
usually generated by `position`, `top`, `left`, `bottom`, or`right`.

`choiceAttributes` are applied to the `div` holding each `choice`.

`header` is put before the input `div` and the `choice` divs.

`renderInputDiv` renders your `input`, if you pass one to `view`.

`renderChoice` renders each choice inside its `div`.

`footer` is put after the `choice` divs.

`wrapper` maps a selected choice to a `msg` for your `update` function.

-}
type alias PopupPicker choice msg =
    { divAttributes : List (Attribute msg)
    , positionAttributes : List (Attribute msg)
    , choiceAttributes : List (Attribute msg)
    , header : Maybe (Html msg)
    , renderInputDiv : Maybe (String -> Html msg)
    , renderChoice : choice -> Html msg
    , footer : Maybe (Html msg)
    , wrapper : choice -> msg
    }


{-| Make a bare-bones `PopupPicker`.

Args are `renderChoice` and `wrapper` properties.

-}
makePopupPicker : (choice -> Html msg) -> (choice -> msg) -> PopupPicker choice msg
makePopupPicker renderChoice wrapper =
    { divAttributes = []
    , positionAttributes = []
    , choiceAttributes = []
    , header = Nothing
    , renderInputDiv = Nothing
    , renderChoice = renderChoice
    , footer = Nothing
    , wrapper = wrapper
    }


{-| Render the popup.

If the first arg is not Nothing, will call `renderInputDiv` to display it.
Sometimes you want to do this on mobile, when the type-in area may be covered
by the picker.

The generated Html is of the form:

    div [ style "position" "absolute"
        , divAttributes
        , positionAttributes
        ]
      [ header
      , renderInputDiv input
      , div [ onClick <| wrapper choice)>
            , choiceAttributes
            ]
          [ renderChoice choice ]
      , ...
      , footer
      ]

-}
view : Maybe String -> List choice -> PopupPicker choice msg -> Html msg
view input choices picker =
    div
        (List.concat
            [ [ style "position" "absolute"
              , style "cursor" "default"
              ]
            , picker.divAttributes
            , picker.positionAttributes
            ]
        )
    <|
        List.concat
            [ [ case picker.header of
                    Nothing ->
                        text ""

                    Just head ->
                        head
              , case input of
                    Nothing ->
                        text ""

                    Just inp ->
                        case picker.renderInputDiv of
                            Nothing ->
                                div [] [ text inp ]

                            Just rid ->
                                rid inp
              ]
            , List.map (renderChoiceDiv picker) choices
            , [ case picker.footer of
                    Nothing ->
                        text ""

                    Just foot ->
                        foot
              ]
            ]


renderChoiceDiv : PopupPicker choice msg -> choice -> Html msg
renderChoiceDiv picker choice =
    div ((onClick <| picker.wrapper choice) :: picker.choiceAttributes)
        [ picker.renderChoice choice ]


{-| `position (x, y)` is the same as `[ left x, top y ]`.
-}
position : ( String, String ) -> List (Attribute msg)
position ( x, y ) =
    [ left x, top y ]


{-| `top x` -> `style "top"  x`
-}
top : String -> Attribute msg
top x =
    style "top" x


{-| `left x` -> `style "left" x`
-}
left : String -> Attribute msg
left x =
    style "left" x


{-| `bottom x` -> `style "bottom x`
-}
bottom : String -> Attribute msg
bottom x =
    style "bottom" x


{-| `right x` -> `style "right" x`
-}
right : String -> Attribute msg
right x =
    style "right" x


{-| Pass a larger integer than any other z-index to make the popup go on top.

This usually goes in the `divAttributes` property of your `PopupPicker`.

`zIndex index` -> `style "z-index" <| String.fromInt index`.

-}
zIndex : Int -> Attribute msg
zIndex index =
    style "z-index" <| String.fromInt index
