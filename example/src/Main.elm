module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Html exposing (Html, b, br, div, h2, input, span, text)
import Html.Attributes exposing (id, size, style, value)
import Html.Events exposing (onInput)
import PopupPicker exposing (PopupPicker)
import Task


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { input : String
    , choices : List Int
    , choice : Maybe Int
    , picker : PopupPicker Int Msg
    , lastElement : Maybe Dom.Element
    }


initialPicker : PopupPicker Int Msg
initialPicker =
    let
        picker =
            PopupPicker.makePopupPicker renderInteger Choose
    in
    { picker
        | divAttributes =
            [ style "border" "1px solid blue"
            , style "width" "5em"
            ]
        , choiceAttributes =
            [ style "border" "1px solid green"
            , style "width" "100%"
            ]
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { input = ""
      , choices = []
      , choice = Nothing
      , picker = initialPicker
      , lastElement = Nothing
      }
    , Cmd.none
    )


renderInteger : Int -> Html msg
renderInteger int =
    span [ style "margin" "3px" ]
        [ text <| String.fromInt int ]


type Msg
    = Choose Int
    | Input String
    | ReceiveElement (Result Dom.Error Dom.Element)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Choose choice ->
            ( { model
                | input = ""
                , choice = Just choice
                , choices = []
              }
            , Cmd.none
            )

        Input string ->
            let
                mdl =
                    { model | input = string, choice = Nothing }
            in
            case String.toInt string of
                Nothing ->
                    ( { mdl | choices = [] }, Cmd.none )

                Just i ->
                    ( { mdl | choices = List.range 1 <| min 20 i }
                    , Task.attempt ReceiveElement <|
                        Dom.getElement lastid
                    )

        ReceiveElement result ->
            let
                element =
                    case result of
                        Err _ ->
                            Nothing

                        Ok el ->
                            Just el
            in
            ( { model | lastElement = element }, Cmd.none )


lastid =
    "last"


view : Model -> Html Msg
view model =
    div [ style "margin" "4em" ]
        [ h2 [] [ text "PopupPicker Example" ]
        , b [] [ text "Number from 1 to 20: " ]
        , input
            [ size 5
            , onInput Input
            , value model.input
            ]
            []
        , br [] []
        , case model.choice of
            Nothing ->
                text ""

            Just choice ->
                Html.span []
                    [ b [] [ text "You chose: " ]
                    , text <| String.fromInt choice
                    ]
        , let
            nothingDiv =
                div [ id lastid ]
                    [ text "Nothing to choose" ]
          in
          case model.choices of
            [] ->
                nothingDiv

            choices ->
                case model.lastElement of
                    Nothing ->
                        nothingDiv

                    Just element ->
                        let
                            el =
                                element.element

                            x =
                                el.x + 20

                            y =
                                el.y + el.height + 20

                            xs =
                                String.fromFloat x ++ "px"

                            ys =
                                String.fromFloat y ++ "px"

                            picker =
                                model.picker

                            picker2 =
                                { picker
                                    | positionAttributes =
                                        PopupPicker.position ( xs, ys )
                                }
                        in
                        div []
                            [ span [ id lastid ]
                                [ text "Click on a number" ]
                            , PopupPicker.view Nothing choices picker2
                            ]
        ]
