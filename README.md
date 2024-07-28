# NaunoKTM/elm-ui-mosaic

This library provides a way to create responsive mosaic layouts with modals in Elm applications using elm-ui.

## Installation

    elm install NaunoKTM/elm-ui-mosaic

## Usage

Here's a basic example of how to use the mosaic:

```elm
import NaunoKTM.Mosaic as Mosaic

-- Define your pictures
pictures : List Mosaic.Picture
pictures =
    [ Mosaic.Picture "1.jpg" { width = 880, height = 609 }
    , Mosaic.Picture "2.jpg" { width = 1200, height = 800 }
    -- Add more pictures...
    ]

-- Initialize the Mosaic in your model
type alias Model =
    { mosaic : Mosaic.Mosaic
    -- Other fields...
    }

init : () -> ( Model, Cmd Msg )
init _ =
    ( { mosaic = Mosaic.init Mosaic.defaultSizeConfig pictures
      -- Initialize other fields...
      }
    , Cmd.none
    )

-- Update your model
type Msg
    = MosaicMsg Mosaic.Msg
    -- Other messages...

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MosaicMsg subMsg ->
            ( { model | mosaic = Mosaic.update subMsg model.mosaic }, Cmd.none )
        -- Handle other messages...

-- Add the view to your layout
view : Model -> Html Msg
view model =
    Element.layout []
        [ column
            [ width fill
            , height fill
            , inFront <| Element.map MosaicMsg <| Mosaic.displayModal model.mosaic
            ]
            [ column
                [ width fill
                , height fill
                , centerX
                , centerY
                , paddingEach { top = 32, bottom = 32, left = 32, right = 32 }
                ]
                [ Element.map MosaicMsg <|
                    Mosaic.viewMosaic model.mosaic
                ]
            ]
        ]

-- Set up subscriptions
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map MosaicMsg (Mosaic.subscriptions model.mosaic)
```

# Live exemple

https://naunoktm-mosaic.lamdera.app/

# License
This project is licensed under the BSD 3-Clause License - see the LICENSE file for details.