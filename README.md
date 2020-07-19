[billstclair/elm-popup-picker](https://package.elm-lang.org/packages/billstclair/elm-popup-picker/latest) is a simple pop-up `div` to choose from a list of possibilities.

    type alias Model =
      { input : String
      , choices : List Integer
      , popup : PopupPicker Integer Msg
      }
      
    init : ( Model, Cmd Msg )
    init =
      { input = ""
      , choices = []
      , popup = makePopPicker Html.text Choose
      }
      
    type Msg
      = Choose Integer
      | Input String
      
    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
      case msg of
        Choose int ->
          let i = Debug.log "You chose" int
          in
          ( { model | input = "" }, Cmd.none )
        Input string ->
          case String.toInt of
            Nothing ->
              ( { model | choices = [] }, Cmd.none )
            Just i ->
              ( { model | choices = List.range 1 i }, Cmd.none )
          
    view : Model -> Html Msg
    view model =
      div []
        [ input [ size 5, onInput Input, value model.input ] []
        , case model.choices of
            [] -> 
              text ""
            choices ->
              let picker = model.picker
                  picker2 =
                    { picker 
                       | positionAttributes =
                           PopupPicker.position (50, 50)
                    }
              PopupPicker.view Nothing choices picker2
        ]
