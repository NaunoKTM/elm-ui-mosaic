# NaunoKTM/elm-ui-mosaic

This library provides a way to create responsive mosaic mosaic layouts with modals in Elm applications using elm-ui.

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

-- In your view function
Mosaic.displayMosaic Mosaic.defaultSizeConfig pictures 0
```

## License

This project is licensed under the BSD 3-Clause License - see the LICENSE file for details.
