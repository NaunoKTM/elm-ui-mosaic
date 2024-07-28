module Mosaic exposing
    ( DisplayConfig, Picture, PictureSize, Modal(..), KeyBoardKey(..), Msg(..)
    , defaultSizeConfig
    , displayModal
    , findPicture, keyDecoder
    , Mosaic, init, subscriptions, update, updateScreenSize, viewMosaic
    )

{-| This library provides a way to create responsive mosaic layouts with a modal in Elm applications using elm-ui.


# Types

@docs DisplayConfig, Picture, PictureSize, Modal, KeyBoardKey, Msg


# Configuration

@docs defaultSizeConfig


# Display Functions

@docs displayMosaic, displayModal


# Utility Functions

@docs findPicture, keyDecoder

-}

import Browser.Events
import Element exposing (..)
import Element.Background as Background
import Element.Events exposing (onClick)
import Element.Font as Font
import Html.Attributes as HA
import Html.Events as HE
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
    = PictureOpen Int


{-| Keyboard key type for navigation.
-}
type KeyBoardKey
    = Left
    | Right
    | Escape
    | Other


{-| Message type for the module.
-}
type Msg
    = NoOpMsg
    | ModalOpen Modal
    | ModalExit
    | ReceiveKeyboardEvent KeyBoardKey


{-| Default configuration for the mosaic display.
-}
defaultSizeConfig : DisplayConfig
defaultSizeConfig =
    { baseWidth = 350
    , baseHeight = 500
    , spacingSize = 8
    }


type alias Mosaic =
    { pictures : List Picture
    , modal : Maybe Modal
    , config : DisplayConfig
    , screenSize : { deviceWidth : Int, deviceHeight : Int }
    }


{-| Initialize a new Mosaic with the given display configuration and list of pictures.
-}
init : DisplayConfig -> List Picture -> Mosaic
init displayConfig pictures =
    { pictures = sortByHeight pictures, modal = Nothing, config = displayConfig, screenSize = { deviceWidth = 0, deviceHeight = 0 } }


{-| Define subscriptions for the keyboard events.
-}
subscriptions : Mosaic -> Sub Msg
subscriptions _ =
    Browser.Events.onKeyDown (D.map ReceiveKeyboardEvent keyDecoder)


{-| Update the Mosaic model based on the received message.
-}
update : Msg -> Mosaic -> Mosaic
update msg model =
    case msg of
        ModalOpen modal ->
            { model | modal = Just modal }

        ModalExit ->
            { model | modal = Nothing }

        NoOpMsg ->
            model

        ReceiveKeyboardEvent direction ->
            case ( direction, model.modal ) of
                ( Left, Just (PictureOpen 0) ) ->
                    model

                ( Left, Just (PictureOpen index) ) ->
                    { model | modal = Just <| PictureOpen (index - 1) }

                ( Right, Just (PictureOpen index) ) ->
                    if index < List.length model.pictures - 1 then
                        { model | modal = Just <| PictureOpen (index + 1) }

                    else
                        model

                ( Escape, _ ) ->
                    { model | modal = Nothing }

                _ ->
                    model


{-| Display a mosaic of pictures.
-}
viewMosaic : Mosaic -> Element Msg
viewMosaic { config, pictures } =
    let
        fixConfig =
            makeItComp config

        displaySizeOfList list =
            el [ width fill, height fill, Background.color (rgba 0 0 0 0.4), letClickThrough ] <|
                el [ centerY, centerX, Font.color (rgb 255 255 255), Font.size 32 ] <|
                    text <|
                        ("+" ++ String.fromInt (List.length list))

        sortedPictures =
            sortByHeight pictures

        calculateHeight : Float -> Float -> Float
        calculateHeight totalItems spacingCount =
            (toFloat fixConfig.baseHeight - (spacingCount * toFloat fixConfig.spacingSize)) / totalItems

        layoutPictures : List Picture -> Element Msg
        layoutPictures pics =
            case pics of
                [] ->
                    none

                [ picture ] ->
                    onePicture fixConfig.baseWidth fixConfig.baseHeight picture 0

                [ picture1, picture2 ] ->
                    let
                        w =
                            round (toFloat fixConfig.baseWidth * 0.5)

                        h =
                            round (calculateHeight 2 1)
                    in
                    row [ spacing fixConfig.spacingSize ]
                        [ onePicture w h picture1 0
                        , onePicture w h picture2 1
                        ]

                [ picture1, picture2, picture3 ] ->
                    let
                        w =
                            round (toFloat fixConfig.baseWidth * 0.5)

                        h1 =
                            fixConfig.baseHeight

                        h2 =
                            round (calculateHeight 2 1)
                    in
                    row [ spacing fixConfig.spacingSize ]
                        [ column [ spacing fixConfig.spacingSize ]
                            [ onePicture w h1 picture1 0 ]
                        , column [ spacing fixConfig.spacingSize ]
                            [ onePicture w h2 picture2 1
                            , onePicture w h2 picture3 2
                            ]
                        ]

                [ picture1, picture2, picture3, picture4 ] ->
                    let
                        w =
                            round (toFloat fixConfig.baseWidth * 0.5)

                        h =
                            round (calculateHeight 2 1)
                    in
                    row [ spacing fixConfig.spacingSize ]
                        [ column [ spacing fixConfig.spacingSize ]
                            [ onePicture w h picture1 0
                            , onePicture w h picture2 1
                            ]
                        , column [ spacing fixConfig.spacingSize ]
                            [ onePicture w h picture3 2
                            , onePicture w h picture4 3
                            ]
                        ]

                [ picture1, picture2, picture3, picture4, picture5 ] ->
                    let
                        w =
                            round (toFloat fixConfig.baseWidth * 0.5)

                        h1 =
                            round (calculateHeight 2 1)

                        h2 =
                            round (calculateHeight 3 2)
                    in
                    row [ spacing fixConfig.spacingSize ]
                        [ column [ spacing fixConfig.spacingSize ]
                            [ onePicture w h1 picture1 0
                            , onePicture w h1 picture2 1
                            ]
                        , column [ spacing fixConfig.spacingSize ]
                            [ onePicture w h2 picture3 2
                            , onePicture w h2 picture4 3
                            , onePicture w h2 picture5 4
                            ]
                        ]

                picture1 :: picture2 :: picture3 :: picture4 :: picture5 :: restOfList ->
                    let
                        w =
                            round (toFloat fixConfig.baseWidth * 0.5)

                        h1 =
                            round (calculateHeight 2 1)

                        h2 =
                            round (calculateHeight 3 2)
                    in
                    row [ spacing fixConfig.spacingSize ]
                        [ column [ spacing fixConfig.spacingSize, alignTop ]
                            [ onePicture w h1 picture1 0
                            , onePicture w h1 picture2 1
                            ]
                        , column [ spacing fixConfig.spacingSize, alignTop ]
                            [ onePicture w h2 picture3 2
                            , onePicture w h2 picture4 3
                            , el [ inFront <| displaySizeOfList restOfList ] <|
                                onePicture w h2 picture5 4
                            ]
                        ]
    in
    el [ centerX ] <| layoutPictures sortedPictures


{-| Find a picture in a list by its index.
-}
findPicture : List Picture -> Int -> Maybe Picture
findPicture pictures pictureId =
    List.getAt pictureId pictures


{-| Display a modal with the selected picture.

`inFront <| Mosaic.displayModal model.mosaic`

-}
displayModal : Mosaic -> Element Msg
displayModal { modal, pictures, screenSize } =
    case modal of
        Nothing ->
            none

        Just (PictureOpen pictureId) ->
            overlayEl <|
                el
                    [ width fill
                    , height fill
                    , centerX
                    , onClick ModalExit
                    , clip
                    ]
                <|
                    let
                        maybePicture =
                            findPicture pictures pictureId
                    in
                    Maybe.withDefault none <|
                        Maybe.map
                            (\picture ->
                                let
                                    -- Calculate available space
                                    availableWidth =
                                        screenSize.deviceWidth - 28

                                    availableHeight =
                                        screenSize.deviceHeight - 80

                                    -- Calculate scaling factor
                                    widthScale =
                                        toFloat availableWidth / toFloat picture.size.width

                                    heightScale =
                                        toFloat availableHeight / toFloat picture.size.height

                                    scale =
                                        min widthScale heightScale

                                    -- Calculate final dimensions
                                    finalWidth =
                                        round (toFloat picture.size.width * scale)

                                    finalHeight =
                                        round (toFloat picture.size.height * scale)

                                    -- Determine if we need to show navigation below
                                    showNavigationBelow =
                                        availableWidth < 1120

                                    navigationRow =
                                        row
                                            [ centerX
                                            , spacing 32
                                            ]
                                            [ previousPic pictureId
                                            , if pictureId < List.length pictures - 1 then
                                                nextPic pictureId

                                              else
                                                none
                                            ]
                                in
                                column
                                    [ centerX
                                    , centerY
                                    , width fill
                                    , height fill
                                    ]
                                    [ if showNavigationBelow then
                                        column [ width fill, height fill ]
                                            [ el
                                                [ centerX
                                                , centerY
                                                , width (px finalWidth)
                                                , height (px finalHeight)
                                                , spacing 32
                                                ]
                                                (image
                                                    [ width (px finalWidth)
                                                    , height (px finalHeight)
                                                    , paddingEach { edges | top = 20 }
                                                    ]
                                                    { description = picture.id, src = picture.id }
                                                )
                                            , navigationRow
                                            ]

                                      else
                                        row [ width fill, height fill ]
                                            [ el [ width (px 100), height fill ] (previousPic pictureId)
                                            , el
                                                [ centerY
                                                , centerX
                                                , width (px finalWidth)
                                                , height (px finalHeight)
                                                , clip
                                                ]
                                                (image
                                                    [ width (px finalWidth)
                                                    , height (px finalHeight)
                                                    , centerY
                                                    , greedyOnClick NoOpMsg
                                                    ]
                                                    { description = picture.id, src = picture.id }
                                                )
                                            , if pictureId < List.length pictures - 1 then
                                                el [ width (px 100), height fill ] (nextPic pictureId)

                                              else
                                                el [ width (px 100), height fill ] none
                                            ]
                                    ]
                            )
                            maybePicture


{-| Decoder for keyboard events.

    Browser.Events.onKeyDown (D.map ReceiveKeyboardEvent keyDecoder)

-}
keyDecoder : D.Decoder KeyBoardKey
keyDecoder =
    D.map toDirection (D.field "key" D.string)


{-| Update the screen size in the Mosaic model.
-}
updateScreenSize : { deviceWidth : Int, deviceHeight : Int } -> Mosaic -> Mosaic
updateScreenSize screenSize model =
    { model | screenSize = screenSize }



-- Helper functions (not exposed)


onePicture : Int -> Int -> Picture -> Int -> Element Msg
onePicture blockWidth blockHeight picture id =
    let
        attrs =
            if toFloat picture.size.height / toFloat blockHeight < toFloat picture.size.width / toFloat blockWidth then
                [ height <| px blockHeight ]

            else
                [ width <| px blockWidth ]
    in
    el [ width <| px blockWidth, height <| px blockHeight, clip ] <|
        el
            [ onClick <| ModalOpen <| PictureOpen id
            , pointer
            , htmlAttribute <| HA.id <| picture.id
            , centerX
            , centerY
            , clip
            ]
        <|
            image
                (clip :: attrs)
                { description = picture.id, src = picture.id }


makeItComp : DisplayConfig -> DisplayConfig
makeItComp displayConfig =
    let
        x =
            displayConfig.spacingSize

        y =
            displayConfig.baseHeight

        n =
            ((y - 2 * x) + 5) // 6

        newY =
            max y (6 * n + 2 * x)
    in
    { displayConfig | baseHeight = newY }


sortByHeight : List Picture -> List Picture
sortByHeight pictures =
    List.sortWith (\pic1 pic2 -> compare pic2.size.height pic1.size.height) pictures


previousPic : Int -> Element Msg
previousPic index =
    if index == 0 then
        el [ width <| px 100, height fill ] none

    else
        el
            [ width <| px 100
            , paddingEach { edges | right = 30, left = 30 }
            , centerY
            , Font.color (rgb 218 212 203)
            , Font.size 67
            , moveUp 3.5
            , pointer
            , greedyOnClick <| ModalOpen <| PictureOpen (index - 1)
            ]
        <|
            text "<"


nextPic : Int -> Element Msg
nextPic index =
    el
        [ width <| px 100
        , paddingEach { edges | right = 30, left = 30 }
        , centerY
        , Font.color (rgb 218 212 203)
        , Font.size 67
        , moveUp 3.5
        , pointer
        , greedyOnClick <| ModalOpen <| PictureOpen (index + 1)
        ]
    <|
        text ">"


overlayEl : Element msg -> Element msg
overlayEl content =
    el
        [ width fill
        , height fill
        , Background.color <| rgba 0 0 0 0.7
        , htmlAttribute <| HA.style "overflow-y" "auto"
        , htmlAttribute <| HA.style "position" "fixed"
        , htmlAttribute <| HA.style "top" "0"
        , htmlAttribute <| HA.style "right" "0"
        , htmlAttribute <| HA.style "bottom" "0"
        , htmlAttribute <| HA.style "left" "0"
        ]
        content


edges : { top : Int, right : Int, bottom : Int, left : Int }
edges =
    { top = 0, right = 0, bottom = 0, left = 0 }


greedyOnClick : msg -> Attribute msg
greedyOnClick msg =
    htmlAttribute <|
        HE.custom "click" (D.succeed { message = msg, stopPropagation = True, preventDefault = True })


letClickThrough : Attribute msg
letClickThrough =
    htmlAttribute <| HA.style "pointer-events" "none"


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
