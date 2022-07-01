module Main exposing (..)

import Browser
import Browser.Events
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode
import Markdown
import Url



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , content : String
    , letter : Char
    , fromDir : String
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( Model key url "" '?' "top"
    , Cmd.none
    )


type Direction
    = Right
    | Left



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | ChangePage Direction
    | GotArticle (Result Http.Error String)
    | PressedLetter Char
    | DoNothing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    if String.contains "/files/" (Url.toString url) then
                        ( model, Nav.load (Url.toString url) )

                    else
                        ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url }, Cmd.none )

        ChangePage direction ->
            let
                newLetter : Char
                newLetter =
                    nextLetter model.letter direction
            in
            ( { model
                | letter = newLetter
                , content = ""
                , fromDir = directionToText direction
              }
            , fetchArticle newLetter
            )

        GotArticle result ->
            case result of
                Ok fullText ->
                    ( { model | content = fullText }, Cmd.none )

                Err _ ->
                    ( { model | content = "There was an error retrieving the content corresponding to the letter " ++ String.fromChar model.letter }, Cmd.none )

        PressedLetter character ->
            ( { model
                | letter = character
                , content = ""
                , fromDir = "bottom"
              }
            , fetchArticle character
            )

        DoNothing ->
            ( model, Cmd.none )


nextLetter : Char -> Direction -> Char
nextLetter letter dir =
    let
        ( firstLetter, lastLetter, nextChar ) =
            case dir of
                Right ->
                    ( 'a', 'z', 1 )

                Left ->
                    ( 'z', 'a', -1 )
    in
    if Char.isAlpha letter == False then
        firstLetter

    else if letter == lastLetter then
        ' '

    else
        Char.fromCode (Char.toCode letter + nextChar)


fetchArticle : Char -> Cmd Msg
fetchArticle letter =
    if letter >= 'a' && letter <= 'z' then
        Http.get
            { url = "/articles/" ++ String.fromChar letter
            , expect = Http.expectString GotArticle
            }

    else
        Cmd.none



-- SUBSCRIPTIONS


type Key
    = Character Char
    | Control String


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onKeyDown keyDecoder


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toKey (Decode.field "key" Decode.string)


toKey : String -> Msg
toKey string =
    case String.uncons string of
        Just ( char, "" ) ->
            if 'a' <= char && char <= 'z' then
                PressedLetter char

            else if 'A' <= char && char <= 'Z' then
                PressedLetter (Char.toLower char)

            else
                PressedLetter ' '

        Just ( c, s ) ->
            if (String.fromChar c ++ s) == "ArrowLeft" then
                ChangePage Left

            else if (String.fromChar c ++ s) == "ArrowRight" then
                ChangePage Right

            else
                DoNothing

        _ ->
            PressedLetter ' '



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Arthur Gossuin"
    , body =
        [ cvButton
        , arrow Left
        , arrow Right
        , if not (Char.isAlpha model.letter) then
            viewInfo

          else
            viewArticle model
        , pageFooter
        ]
    }


viewInfo : Html Msg
viewInfo =
    div [ class "info w3-animate-fading" ] [ text "Press a letter to start" ]


viewArticle : Model -> Html Msg
viewArticle model =
    div
        [ class "article"
        , if String.isEmpty model.content then
            class ""

          else
            class ("w3-animate-" ++ model.fromDir)
        ]
        [ toMarkdown model.content ]


type alias Options =
    { githubFlavored : Maybe { tables : Bool, breaks : Bool }
    , defaultHighlighting : Maybe String
    , sanitize : Bool
    , smartypants : Bool
    }


myOptions : Options
myOptions =
    { githubFlavored = Just { tables = True, breaks = True }
    , defaultHighlighting = Nothing
    , sanitize = False
    , smartypants = True
    }


toMarkdown : String -> Html Msg
toMarkdown userInput =
    Markdown.toHtmlWith myOptions [] userInput


cvButton : Html msg
cvButton =
    div [ class "download-button" ]
        [ a [ href "../files/CV_ARTHUR_GOSSUIN_JUNE2022.pdf" ]
            [ div [ style "text-decoration" "none" ]
                [ i [ class "fa fa-file-text w3-margin-right" ] []
                , text "click for résumé"
                ]
            ]
        ]


pageFooter : Html msg
pageFooter =
    footer
        [ class "w3-center w3-padding-32" ]
        [ div
            [ class "w3-xlarge w3-section text-decoration-none" ]
            [ faIconLink "github" "https://github.com/Artygo8"
            , text " "
            , faIconLink "code" "https://www.codingame.com/profile/f47f29046aba3b04ae6401291e49b6246631973"
            , text " "
            , faIconLink "linkedin" "https://www.linkedin.com/in/arthur-gossuin-848273105/"
            ]
        ]


faIconLink : String -> String -> Html msg
faIconLink iconName link =
    a [ href link ] [ i [ class ("fa w3-hover-opacity fa-" ++ iconName) ] [] ]


arrow : Direction -> Html Msg
arrow dir =
    div [ class "w3-center arrow-button" ]
        [ div [ onClick (ChangePage dir) ]
            [ i
                [ class ("fa fa-chevron-" ++ directionToText dir)
                , class ("arrow arrow-" ++ directionToText dir)
                ]
                []
            ]
        ]


directionToText : Direction -> String
directionToText dir =
    case dir of
        Right ->
            "right"

        Left ->
            "left"
