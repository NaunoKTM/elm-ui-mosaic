module NaunoKTM.Mosaic exposing
    ( DisplayConfig, Picture, PictureSize, Modal(..), KeyBoardKey(..), Msg(..)
    , defaultSizeConfig
    , displayMosaic, displayModal
    , findPicture, keyDecoder
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
    = PictureOpen Int Int


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


{-| Default configuration for the mosaic display.
-}
defaultSizeConfig : DisplayConfig
defaultSizeConfig =
    { baseWidth = 350
    , baseHeight = 500
    , spacingSize = 8
    }


{-| Display a mosaic of pictures.

    displayMosaic defaultSizeConfig myPictures 0

-}
displayMosaic : DisplayConfig -> List Picture -> Int -> Element Msg
displayMosaic config pictures listIndex =
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
                    onePicture fixConfig.baseWidth fixConfig.baseHeight picture listIndex 0

                [ picture1, picture2 ] ->
                    let
                        w =
                            round (toFloat fixConfig.baseWidth * 0.5)

                        h =
                            round (calculateHeight 2 1)
                    in
                    row [ spacing fixConfig.spacingSize ]
                        [ onePicture w h picture1 listIndex 0
                        , onePicture w h picture2 listIndex 1
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
                            [ onePicture w h1 picture1 listIndex 0 ]
                        , column [ spacing fixConfig.spacingSize ]
                            [ onePicture w h2 picture2 listIndex 1
                            , onePicture w h2 picture3 listIndex 2
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
                            [ onePicture w h picture1 listIndex 0
                            , onePicture w h picture2 listIndex 1
                            ]
                        , column [ spacing fixConfig.spacingSize ]
                            [ onePicture w h picture3 listIndex 2
                            , onePicture w h picture4 listIndex 3
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
                            [ onePicture w h1 picture1 listIndex 0
                            , onePicture w h1 picture2 listIndex 1
                            ]
                        , column [ spacing fixConfig.spacingSize ]
                            [ onePicture w h2 picture3 listIndex 2
                            , onePicture w h2 picture4 listIndex 3
                            , onePicture w h2 picture5 listIndex 4
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
                            [ onePicture w h1 picture1 listIndex 0
                            , onePicture w h1 picture2 listIndex 1
                            ]
                        , column [ spacing fixConfig.spacingSize, alignTop ]
                            [ onePicture w h2 picture3 listIndex 2
                            , onePicture w h2 picture4 listIndex 3
                            , el [ inFront <| displaySizeOfList restOfList ] <|
                                onePicture w h2 picture5 listIndex 4
                            ]
                        ]
    in
    el [ centerX ] <| layoutPictures sortedPictures


{-| Find a picture in a list by its index.

    findPicture myPictures 2

-}
findPicture : List Picture -> Int -> Maybe Picture
findPicture pictures pictureId =
    List.getAt pictureId pictures


{-| Display a modal with the selected picture.

    displayModal (PictureOpen 0 2) model

-}
displayModal : Modal -> { a | deviceWidth : Int, deviceHeight : Int } -> Element Msg
displayModal modal model =
    overlayEl <|
        el
            [ width fill
            , height fill
            , centerX
            , onClick ModalExit
            , clip
            ]
        <|
            case modal of
                PictureOpen listIndex pictureId ->
                    let
                        maybePicture =
                            findPicture (getListFromIndex listIndex) pictureId
                    in
                    Maybe.withDefault none <|
                        Maybe.map
                            (\picture ->
                                let
                                    pictures =
                                        getListFromIndex listIndex

                                    -- Calculate available space
                                    availableWidth =
                                        model.deviceWidth - 300

                                    availableHeight =
                                        model.deviceHeight - 80

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
                                            [ previousPic listIndex pictureId
                                            , nextPic listIndex pictureId
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
                                            [ el [ width (px 100), height fill ] (previousPic listIndex pictureId)
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
                                                el [ width (px 100), height fill ] (nextPic listIndex pictureId)

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



-- Helper functions (not exposed)


onePicture : Int -> Int -> Picture -> Int -> Int -> Element Msg
onePicture blockWidth blockHeight picture listIndex id =
    let
        attrs =
            if toFloat picture.size.height / toFloat blockHeight < toFloat picture.size.width / toFloat blockWidth then
                [ height <| px blockHeight ]

            else
                [ width <| px blockWidth ]
    in
    el [ width <| px blockWidth, height <| px blockHeight, clip ] <|
        el
            [ onClick <| ModalOpen <| PictureOpen listIndex id
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


previousPic : Int -> Int -> Element Msg
previousPic listIndex index =
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
            , greedyOnClick <| ModalOpen <| PictureOpen listIndex (index - 1)
            ]
        <|
            text "<"


nextPic : Int -> Int -> Element Msg
nextPic listIndex index =
    if index == List.length (getListFromIndex listIndex) - 1 then
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
            , greedyOnClick <| ModalOpen <| PictureOpen listIndex (index + 1)
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


getListFromIndex : Int -> List Picture
getListFromIndex index =
    case index of
        0 ->
            sortByHeight firstList

        _ ->
            []


firstList : List Picture
firstList =
    [ Picture "1.jpg" { width = 880, height = 609 }
    , Picture "2.jpg" { width = 1200, height = 800 }
    , Picture "3.jpg" { width = 845, height = 321 }
    , Picture "4.jpg" { width = 1080, height = 474 }
    , Picture "5.jpg" { width = 1024, height = 576 }
    , Picture "6.jpg" { width = 1080, height = 474 }
    , Picture "7.jpg" { width = 1024, height = 576 }
    , Picture "8.jpg" { width = 1280, height = 853 }
    ]
