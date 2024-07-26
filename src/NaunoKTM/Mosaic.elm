module NaunoKTM.Mosaic exposing
    ( DisplayConfig
    , Picture
    , PictureSize
    , Modal(..)
    , KeyBoardKey(..)
    , displayMosaic
    , defaultSizeConfig
    , findPicture
    , displayModal
    , keyDecoder
    )

{-| This library provides a way to create responsive mosaic layouts with a modal in Elm applications using elm-ui.

# Types
@docs DisplayConfig, Picture, PictureSize, Modal, KeyBoardKey

# Configuration
@docs defaultSizeConfig

# Display Functions
@docs displayMosaic, displayModal

# Utility Functions
@docs findPicture, keyDecoder

-}

import Element exposing (..)
import Element.Background as Background
import Element.Events exposing (onClick)
import Element.Font as Font
import Html.Attributes as HA
import Json.Decode as D
import List.Extra as List


{-| Configuration for the display of the mosaic.
-}
type alias DisplayConfig =
    { baseWidth : Int
    , baseHeight : Int
    , spacingSize : Int
    }


{-| Representation of an image in the mosaic.
-}
type alias Picture =
    { id : String
    , size : PictureSize
    }


{-| Size of a picture.
-}
type alias PictureSize =
    { width : Int
    , height : Int
    }


{-| Modal type for opening pictures.
-}
type Modal
    = PictureOpen Int Int


{-| Keyboard key type for navigation.
-}
type KeyBoardKey
    = Left
    | Right
    | Escape
    | Other


{-| Default configuration for the mosaic display.
-}
defaultSizeConfig : DisplayConfig
defaultSizeConfig =
    { baseWidth = 350
    , baseHeight = 500
    , spacingSize = 8
    }


{-| Display a mosaic of pictures.
-}
displayMosaic : DisplayConfig -> List Picture -> Int -> Element msg
displayMosaic config pictures listIndex =
    -- Implementation here
    text "Implement displayMosaic"


{-| Find a picture in a list by its index.
-}
findPicture : List Picture -> Int -> Maybe Picture
findPicture pictures pictureId =
    List.getAt pictureId pictures


{-| Display a modal with the selected picture.
-}
displayModal : Modal -> { deviceWidth : Int, deviceHeight : Int } -> Element msg
displayModal modal model =
    -- Implementation here
    text "Implement displayModal"


{-| Decoder for keyboard events.
-}
keyDecoder : D.Decoder KeyBoardKey
keyDecoder =
    D.map toDirection (D.field "key" D.string)


toDirection : String -> KeyBoardKey
toDirection string =
    case string of
        "ArrowLeft" ->
            Left

        "ArrowRight" ->
            Right

        "Escape" ->
            Escape

        _ ->
            Other
