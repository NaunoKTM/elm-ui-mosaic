module EffectMosaics exposing
    ( DisplayConfig
    , Picture
    , PictureSize
    , Msg(..)
    , Mosaics
    , Modal
    , KeyBoardKey
    , init
    , subscriptions
    , update
    , defaultSizeConfig
    , viewMosaics
    , displayModal
    , updateScreenSize
    , takeFullscreenSize
    )

{-| This library provides a way to create responsive Mosaics layouts with a modal in Elm applications using elm-ui.


# Types

@docs DisplayConfig
@docs Picture
@docs PictureSize
@docs Msg
@docs Mosaics
@docs Modal
@docs KeyBoardKey


# Basics

@docs init
@docs subscriptions
@docs update


# Configuration

@docs defaultSizeConfig


# Display Functions

@docs viewMosaics
@docs displayModal


# Utility Functions

@docs updateScreenSize
@docs takeFullscreenSize

-}

import Effect.Browser.Events as BrowserEvents
import Effect.Command exposing (FrontendOnly)
import Effect.Subscription exposing (Subscription)
import Element exposing (Attribute, Element, alignTop, centerX, centerY, clip, column, el, fill, height, htmlAttribute, image, inFront, moveUp, none, paddingEach, pointer, px, rgb, rgba, row, spacing, text, width)
import Element.Background as Background
import Element.Events exposing (onClick)
import Element.Font as Font
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as D
import List.Extra as List


{-| Configuration for the display of the Mosaics.
-}
type alias DisplayConfig =
    { baseWidth : Int
    , baseHeight : Int
    , spacingSize : Int
    }


{-| Representation of an image in the Mosaics.
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
    | ModalClosed


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
    | ModalOpen Int Int
    | ModalExit
    | ReceiveKeyboardEvent KeyBoardKey


{-| Default configuration for the Mosaics display.
-}
defaultSizeConfig : DisplayConfig
defaultSizeConfig =
    { baseWidth = 350
    , baseHeight = 500
    , spacingSize = 8
    }


{-| Display a list of Mosaics with each of the given list of pictures.
-}
type alias Mosaics =
    { albums : List (List Picture)
    , modal : Modal
    , config : DisplayConfig
    , screenSize : { deviceWidth : Int, deviceHeight : Int }
    }


{-| Initialize a new Mosaics with the given display configuration and list of pictures.
-}
init : DisplayConfig -> List (List Picture) -> Mosaics
init displayConfig albums =
    { albums = albums |> List.map sortByHeight
    , modal = ModalClosed
    , config = displayConfig
    , screenSize = { deviceWidth = 0, deviceHeight = 0 }
    }


{-| Define subscriptions for the keyboard events.
-}
subscriptions : Mosaics -> Subscription FrontendOnly Msg
subscriptions _ =
    BrowserEvents.onKeyDown (D.map ReceiveKeyboardEvent keyDecoder)


{-| Update the Mosaics model based on the received message.

    type Msg
        = MosaicsMsg Mosaics.Msg
        | (...)

    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
        case msg of
            MosaicsMsg subMsg ->
                ( { model | Mosaics = Mosaics.update subMsg model.Mosaics }, Cmd.none )

    -- Handle other messages...

-}
update : Msg -> Mosaics -> Mosaics
update msg model =
    case msg of
        ModalOpen albumIndex pictureId ->
            { model | modal = PictureOpen albumIndex pictureId }

        ModalExit ->
            { model | modal = ModalClosed }

        NoOpMsg ->
            model

        ReceiveKeyboardEvent direction ->
            case ( direction, model.modal ) of
                ( Left, PictureOpen _ 0 ) ->
                    model

                ( Left, PictureOpen albumIndex index ) ->
                    { model | modal = PictureOpen albumIndex (index - 1) }

                ( Right, PictureOpen albumIndex index ) ->
                    let
                        currentAlbum =
                            List.getAt albumIndex model.albums
                    in
                    case currentAlbum of
                        Nothing ->
                            model

                        Just currentAlbum_ ->
                            if index < List.length currentAlbum_ - 1 then
                                { model | modal = PictureOpen albumIndex (index + 1) }

                            else
                                model

                ( Escape, _ ) ->
                    { model | modal = ModalClosed }

                _ ->
                    model


{-| Display a Mosaics of pictures.
-}
viewMosaics : Mosaics -> Int -> Element Msg
viewMosaics { config, albums } albumIndex =
    let
        currentAlbum : Maybe (List Picture)
        currentAlbum =
            List.getAt albumIndex albums
    in
    case currentAlbum of
        Nothing ->
            none

        Just album ->
            layoutSingleMosaic config album albumIndex


layoutSingleMosaic : DisplayConfig -> List Picture -> Int -> Element Msg
layoutSingleMosaic config pictures albumIndex =
    let
        fixConfig : DisplayConfig
        fixConfig =
            makeItComp config

        displaySizeOfList : List a -> Element msg
        displaySizeOfList list =
            el [ width fill, height fill, Background.color (rgba 0 0 0 0.4), letClickThrough ] <|
                el [ centerY, centerX, Font.color (rgb 255 255 255), Font.size 32 ] <|
                    text <|
                        ("+" ++ String.fromInt (List.length list))

        calculateHeight : Float -> Float -> Float
        calculateHeight totalItems spacingCount =
            (toFloat fixConfig.baseHeight - (spacingCount * toFloat fixConfig.spacingSize)) / totalItems

        layoutPictures : List Picture -> Element Msg
        layoutPictures pics =
            case pics of
                [] ->
                    none

                [ picture ] ->
                    onePicture fixConfig.baseWidth fixConfig.baseHeight picture albumIndex 0

                [ picture1, picture2 ] ->
                    let
                        w : Int
                        w =
                            round (toFloat fixConfig.baseWidth * 0.5)

                        h : Int
                        h =
                            round (calculateHeight 2 1)
                    in
                    row [ spacing fixConfig.spacingSize ]
                        [ onePicture w h picture1 albumIndex 0
                        , onePicture w h picture2 albumIndex 1
                        ]

                [ picture1, picture2, picture3 ] ->
                    let
                        w : Int
                        w =
                            round (toFloat fixConfig.baseWidth * 0.5)

                        h1 : Int
                        h1 =
                            fixConfig.baseHeight

                        h2 : Int
                        h2 =
                            round (calculateHeight 2 1)
                    in
                    row [ spacing fixConfig.spacingSize ]
                        [ column [ spacing fixConfig.spacingSize ]
                            [ onePicture w h1 picture1 albumIndex 0 ]
                        , column [ spacing fixConfig.spacingSize ]
                            [ onePicture w h2 picture2 albumIndex 1
                            , onePicture w h2 picture3 albumIndex 2
                            ]
                        ]

                [ picture1, picture2, picture3, picture4 ] ->
                    let
                        w : Int
                        w =
                            round (toFloat fixConfig.baseWidth * 0.5)

                        h : Int
                        h =
                            round (calculateHeight 2 1)
                    in
                    row [ spacing fixConfig.spacingSize ]
                        [ column [ spacing fixConfig.spacingSize ]
                            [ onePicture w h picture1 albumIndex 0
                            , onePicture w h picture2 albumIndex 1
                            ]
                        , column [ spacing fixConfig.spacingSize ]
                            [ onePicture w h picture3 albumIndex 2
                            , onePicture w h picture4 albumIndex 3
                            ]
                        ]

                [ picture1, picture2, picture3, picture4, picture5 ] ->
                    let
                        w : Int
                        w =
                            round (toFloat fixConfig.baseWidth * 0.5)

                        h1 : Int
                        h1 =
                            round (calculateHeight 2 1)

                        h2 : Int
                        h2 =
                            round (calculateHeight 3 2)
                    in
                    row [ spacing fixConfig.spacingSize ]
                        [ column [ spacing fixConfig.spacingSize ]
                            [ onePicture w h1 picture1 albumIndex 0
                            , onePicture w h1 picture2 albumIndex 1
                            ]
                        , column [ spacing fixConfig.spacingSize ]
                            [ onePicture w h2 picture3 albumIndex 2
                            , onePicture w h2 picture4 albumIndex 3
                            , onePicture w h2 picture5 albumIndex 4
                            ]
                        ]

                picture1 :: picture2 :: picture3 :: picture4 :: picture5 :: restOfList ->
                    let
                        w : Int
                        w =
                            round (toFloat fixConfig.baseWidth * 0.5)

                        h1 : Int
                        h1 =
                            round (calculateHeight 2 1)

                        h2 : Int
                        h2 =
                            round (calculateHeight 3 2)
                    in
                    row [ spacing fixConfig.spacingSize ]
                        [ column [ spacing fixConfig.spacingSize, alignTop ]
                            [ onePicture w h1 picture1 albumIndex 0
                            , onePicture w h1 picture2 albumIndex 1
                            ]
                        , column [ spacing fixConfig.spacingSize, alignTop ]
                            [ onePicture w h2 picture3 albumIndex 2
                            , onePicture w h2 picture4 albumIndex 3
                            , el [ inFront <| displaySizeOfList restOfList ] <|
                                onePicture w h2 picture5 albumIndex 4
                            ]
                        ]
    in
    el [ centerX ] <| layoutPictures pictures



-- viewMosaics : Mosaics -> Element Msg
-- viewMosaics { config, pictures } =
--     let
--         fixConfig : DisplayConfig
--         fixConfig =
--             makeItComp config
--         displaySizeOfList : List a -> Element msg
--         displaySizeOfList list =
--             el [ width fill, height fill, Background.color (rgba 0 0 0 0.4), letClickThrough ] <|
--                 el [ centerY, centerX, Font.color (rgb 255 255 255), Font.size 32 ] <|
--                     text <|
--                         ("+" ++ String.fromInt (List.length list))
--         sortedPictures : List Picture
--         sortedPictures =
--             sortByHeight pictures
--         calculateHeight : Float -> Float -> Float
--         calculateHeight totalItems spacingCount =
--             (toFloat fixConfig.baseHeight - (spacingCount * toFloat fixConfig.spacingSize)) / totalItems
--         layoutPictures : List Picture -> Element Msg
--         layoutPictures pics =
--             case pics of
--                 [] ->
--                     none
--                 [ picture ] ->
--                     onePicture fixConfig.baseWidth fixConfig.baseHeight picture 0
--                 [ picture1, picture2 ] ->
--                     let
--                         w : Int
--                         w =
--                             round (toFloat fixConfig.baseWidth * 0.5)
--                         h : Int
--                         h =
--                             round (calculateHeight 2 1)
--                     in
--                     row [ spacing fixConfig.spacingSize ]
--                         [ onePicture w h picture1 0
--                         , onePicture w h picture2 1
--                         ]
--                 [ picture1, picture2, picture3 ] ->
--                     let
--                         w : Int
--                         w =
--                             round (toFloat fixConfig.baseWidth * 0.5)
--                         h1 : Int
--                         h1 =
--                             fixConfig.baseHeight
--                         h2 : Int
--                         h2 =
--                             round (calculateHeight 2 1)
--                     in
--                     row [ spacing fixConfig.spacingSize ]
--                         [ column [ spacing fixConfig.spacingSize ]
--                             [ onePicture w h1 picture1 0 ]
--                         , column [ spacing fixConfig.spacingSize ]
--                             [ onePicture w h2 picture2 1
--                             , onePicture w h2 picture3 2
--                             ]
--                         ]
--                 [ picture1, picture2, picture3, picture4 ] ->
--                     let
--                         w : Int
--                         w =
--                             round (toFloat fixConfig.baseWidth * 0.5)
--                         h : Int
--                         h =
--                             round (calculateHeight 2 1)
--                     in
--                     row [ spacing fixConfig.spacingSize ]
--                         [ column [ spacing fixConfig.spacingSize ]
--                             [ onePicture w h picture1 0
--                             , onePicture w h picture2 1
--                             ]
--                         , column [ spacing fixConfig.spacingSize ]
--                             [ onePicture w h picture3 2
--                             , onePicture w h picture4 3
--                             ]
--                         ]
--                 [ picture1, picture2, picture3, picture4, picture5 ] ->
--                     let
--                         w : Int
--                         w =
--                             round (toFloat fixConfig.baseWidth * 0.5)
--                         h1 : Int
--                         h1 =
--                             round (calculateHeight 2 1)
--                         h2 : Int
--                         h2 =
--                             round (calculateHeight 3 2)
--                     in
--                     row [ spacing fixConfig.spacingSize ]
--                         [ column [ spacing fixConfig.spacingSize ]
--                             [ onePicture w h1 picture1 0
--                             , onePicture w h1 picture2 1
--                             ]
--                         , column [ spacing fixConfig.spacingSize ]
--                             [ onePicture w h2 picture3 2
--                             , onePicture w h2 picture4 3
--                             , onePicture w h2 picture5 4
--                             ]
--                         ]
--                 picture1 :: picture2 :: picture3 :: picture4 :: picture5 :: restOfList ->
--                     let
--                         w : Int
--                         w =
--                             round (toFloat fixConfig.baseWidth * 0.5)
--                         h1 : Int
--                         h1 =
--                             round (calculateHeight 2 1)
--                         h2 : Int
--                         h2 =
--                             round (calculateHeight 3 2)
--                     in
--                     row [ spacing fixConfig.spacingSize ]
--                         [ column [ spacing fixConfig.spacingSize, alignTop ]
--                             [ onePicture w h1 picture1 0
--                             , onePicture w h1 picture2 1
--                             ]
--                         , column [ spacing fixConfig.spacingSize, alignTop ]
--                             [ onePicture w h2 picture3 2
--                             , onePicture w h2 picture4 3
--                             , el [ inFront <| displaySizeOfList restOfList ] <|
--                                 onePicture w h2 picture5 4
--                             ]
--                         ]
--     in
--     el [ centerX ] <| layoutPictures sortedPictures


{-| Display a modal with the selected picture.

`inFront <| Mosaics.displayModal model.Mosaics`

-- case modal of
-- PictureOpen artId ->
-- row [ width fill, height fill ] <|
-- let
-- maybeArt =
-- maybePicture =
-- findPicture (getListFromIndex) artId
-- in
-- Maybe.withDefault [] <|

-}
displayModal : Mosaics -> Element Msg
displayModal { modal, albums, screenSize } =
    case modal of
        ModalClosed ->
            none

        PictureOpen albumIndex pictureId ->
            case List.getAt albumIndex albums of
                Nothing ->
                    none

                Just currentAlbum ->
                    overlayEl <|
                        el
                            [ width fill
                            , height fill
                            , centerX
                            , onClick ModalExit
                            , clip
                            ]
                            (findPicture currentAlbum pictureId
                                |> Maybe.map
                                    (\picture ->
                                        let
                                            availableWidth : Int
                                            availableWidth =
                                                if showNavigationBelow then
                                                    screenSize.deviceWidth - 28

                                                else
                                                    screenSize.deviceWidth - 300

                                            availableHeight : Int
                                            availableHeight =
                                                screenSize.deviceHeight - 80

                                            widthScale : Float
                                            widthScale =
                                                toFloat availableWidth / toFloat picture.size.width

                                            heightScale : Float
                                            heightScale =
                                                toFloat availableHeight / toFloat picture.size.height

                                            scale : Float
                                            scale =
                                                min widthScale heightScale

                                            finalWidth : Int
                                            finalWidth =
                                                round (toFloat picture.size.width * scale)

                                            finalHeight : Int
                                            finalHeight =
                                                round (toFloat picture.size.height * scale)

                                            showNavigationBelow : Bool
                                            showNavigationBelow =
                                                screenSize.deviceWidth < 1120

                                            navigationRow : Element Msg
                                            navigationRow =
                                                row
                                                    [ centerX
                                                    , spacing 32
                                                    ]
                                                    [ previousPic albumIndex pictureId
                                                    , if pictureId < List.length currentAlbum - 1 then
                                                        nextPic albumIndex pictureId

                                                      else
                                                        el [ width (px 100), height fill ] none
                                                    ]
                                        in
                                        if showNavigationBelow then
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
                                                [ el [ width (px 100), height fill ] (previousPic albumIndex pictureId)
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
                                                , if pictureId < List.length currentAlbum - 1 then
                                                    el [ width (px 100), height fill ] (nextPic albumIndex pictureId)

                                                  else
                                                    el [ width (px 100), height fill ] none
                                                ]
                                    )
                                |> Maybe.withDefault none
                            )



-- displayModal : Mosaics -> Element Msg
-- displayModal { modal, pictures, screenSize } =
--     case modal of
--         ModalClosed ->
--             none
--         PictureOpen pictureId ->
--             overlayEl <|
--                 el
--                     [ width fill
--                     , height fill
--                     , centerX
--                     , onClick ModalExit
--                     , clip
--                     ]
--                     (findPicture pictures pictureId
--                         |> Maybe.map
--                             (\picture ->
--                                 let
--                                     -- Calculate available space
--                                     availableWidth : Int
--                                     availableWidth =
--                                         if showNavigationBelow then
--                                             screenSize.deviceWidth - 28
--                                         else
--                                             screenSize.deviceWidth - 300
--                                     availableHeight : Int
--                                     availableHeight =
--                                         screenSize.deviceHeight - 80
--                                     -- Calculate scaling factor
--                                     widthScale : Float
--                                     widthScale =
--                                         toFloat availableWidth / toFloat picture.size.width
--                                     heightScale : Float
--                                     heightScale =
--                                         toFloat availableHeight / toFloat picture.size.height
--                                     scale : Float
--                                     scale =
--                                         min widthScale heightScale
--                                     -- Calculate final dimensions
--                                     finalWidth : Int
--                                     finalWidth =
--                                         round (toFloat picture.size.width * scale)
--                                     finalHeight : Int
--                                     finalHeight =
--                                         round (toFloat picture.size.height * scale)
--                                     -- Determine if we need to show navigation below
--                                     showNavigationBelow : Bool
--                                     showNavigationBelow =
--                                         screenSize.deviceWidth < 1120
--                                 in
--                                 column
--                                     [ centerX
--                                     , centerY
--                                     , width fill
--                                     , height fill
--                                     ]
--                                     [ if showNavigationBelow then
--                                         let
--                                             navigationRow : Element Msg
--                                             navigationRow =
--                                                 row
--                                                     [ centerX
--                                                     , spacing 32
--                                                     ]
--                                                     [ previousPic pictureId
--                                                     , if pictureId < List.length pictures - 1 then
--                                                         nextPic pictureId
--                                                       else
--                                                         el [ width (px 100), height fill ] none
--                                                     ]
--                                         in
--                                         column [ width fill, height fill ]
--                                             [ el
--                                                 [ centerX
--                                                 , centerY
--                                                 , width (px finalWidth)
--                                                 , height (px finalHeight)
--                                                 , spacing 32
--                                                 ]
--                                                 (image
--                                                     [ width (px finalWidth)
--                                                     , height (px finalHeight)
--                                                     , paddingEach { edges | top = 20 }
--                                                     ]
--                                                     { description = picture.id, src = picture.id }
--                                                 )
--                                             , navigationRow
--                                             ]
--                                       else
--                                         row [ width fill, height fill ]
--                                             [ el [ width (px 100), height fill ] (previousPic pictureId)
--                                             , el
--                                                 [ centerY
--                                                 , centerX
--                                                 , width (px finalWidth)
--                                                 , height (px finalHeight)
--                                                 , clip
--                                                 ]
--                                                 (image
--                                                     [ width (px finalWidth)
--                                                     , height (px finalHeight)
--                                                     , centerY
--                                                     , greedyOnClick NoOpMsg
--                                                     ]
--                                                     { description = picture.id, src = picture.id }
--                                                 )
--                                             , if pictureId < List.length pictures - 1 then
--                                                 el [ width (px 100), height fill ] (nextPic pictureId)
--                                               else
--                                                 el [ width (px 100), height fill ] none
--                                             ]
--                                     ]
--                             )
--                         |> Maybe.withDefault none
--                     )
-- getListFromIndex : Int -> List Picture
-- getListFromIndex index =
--     case index of
--         0 ->
--             sortByHeight firstList
--         1 ->
--             sortByHeight secondList
--         2 ->
--             sortByHeight thirdList
--         _ ->
--             []
--, el [ width <| fillPortion 3 ] <|
-- displayPicturesGeneric homeHabitatImages 0


{-| Update the screen size in the Mosaics model.
-}
updateScreenSize : { a | width : Int, height : Int } -> Mosaics -> Mosaics
updateScreenSize screenSize model =
    { model | screenSize = { deviceWidth = screenSize.width, deviceHeight = screenSize.height } }



-- Helper functions (not exposed)


onePicture : Int -> Int -> Picture -> Int -> Int -> Element Msg
onePicture blockWidth blockHeight picture albumIndex pictureId =
    let
        attrs : List (Attribute msg)
        attrs =
            if toFloat picture.size.height / toFloat blockHeight < toFloat picture.size.width / toFloat blockWidth then
                [ height <| px blockHeight ]

            else
                [ width <| px blockWidth ]
    in
    el [ width <| px blockWidth, height <| px blockHeight, clip ] <|
        el
            [ onClick (ModalOpen albumIndex pictureId)
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


{-| Find a picture in a list by its index.
-}
findPicture : List Picture -> Int -> Maybe Picture
findPicture pictures pictureId =
    List.getAt pictureId pictures


{-| Decoder for keyboard events.

    Browser.Events.onKeyDown (D.map ReceiveKeyboardEvent keyDecoder)

-}
keyDecoder : D.Decoder KeyBoardKey
keyDecoder =
    D.map toDirection (D.field "key" D.string)


makeItComp : DisplayConfig -> DisplayConfig
makeItComp ({ spacingSize, baseHeight } as displayConfig) =
    { displayConfig
        | baseHeight =
            max baseHeight (6 * (((baseHeight - 2 * spacingSize) + 5) // 6) + 2 * spacingSize)
    }


sortByHeight : List Picture -> List Picture
sortByHeight pictures =
    List.sortWith (\pic1 pic2 -> compare pic2.size.height pic1.size.height) pictures


previousPic : Int -> Int -> Element Msg
previousPic albumIndex index =
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
            , greedyOnClick <| ModalOpen albumIndex (index - 1)
            ]
        <|
            text "<"


nextPic : Int -> Int -> Element Msg
nextPic albumIndex index =
    el
        [ width <| px 100
        , paddingEach { edges | right = 30, left = 30 }
        , centerY
        , Font.color (rgb 218 212 203)
        , Font.size 67
        , moveUp 3.5
        , pointer
        , greedyOnClick <| ModalOpen albumIndex (index + 1)
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


{-| Take the fullscreen size of the Mosaics.
-}
takeFullscreenSize : Mosaics -> Mosaics
takeFullscreenSize ({ config } as mosaics) =
    { mosaics
        | config =
            { config
                | baseWidth = mosaics.screenSize.deviceWidth - config.spacingSize
                , baseHeight = mosaics.screenSize.deviceHeight - config.spacingSize
            }
    }



-- {-| Inverse baseWidth and baseHeight if the screen is in landscape mode.
-- -}
-- makeItResponsive : Mosaics -> Mosaics
-- makeItResponsive ({ config } as Mosaics) =
--     if Mosaics.screenSize.deviceWidth > Mosaics.screenSize.deviceHeight then
--         { Mosaics
--             | config =
--                 { config
--                     | baseWidth = config.baseHeight
--                     , baseHeight = config.baseWidth
--                 }
--         }
--     else
--         Mosaics
