
module NaunoKTM.Mosaic exposing (DisplayConfig, Picture, displayPicturesGenericPhone, defaultSizeConfig)

{-| This library provides a way to create mosaic-style image layouts in Elm applications.

# Types
@docs DisplayConfig, Picture

# Configuration
@docs defaultSizeConfig

# Display Functions
@docs displayPicturesGenericPhone

-}

import Element exposing (..)
import Element.Background as Background
import Element.Events exposing (onClick)
import Element.Font as Font
import Html.Attributes as HA


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
    , size : { width : Int, height : Int }
    }


{-| Default configuration for the mosaic display.

    defaultSizeConfig =
        { baseWidth = 350
        , baseHeight = 500
        , spacingSize = 8
        }
-}
defaultSizeConfig : DisplayConfig
defaultSizeConfig =
    { baseWidth = 350
    , baseHeight = 500
    , spacingSize = 8
    }


{-| Display a mosaic of pictures optimized for phone screens.

    displayPicturesGenericPhone defaultSizeConfig myPictures 0

-}
displayPicturesGenericPhone : DisplayConfig -> List Picture -> Int -> Element msg
displayPicturesGenericPhone config pictures listIndex =
    -- Your implementation here
    text "Implement me!"

