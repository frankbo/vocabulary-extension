module Main exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Html exposing (Html, button, div, form, h1, input, span, text)
import Html.Attributes exposing (action, method, placeholder, type_, value)
import Html.Events exposing (onInput)
import Http exposing (get)
import Json.Decode as Decode exposing (Decoder, int, string)
import Json.Decode.Pipeline exposing (required)
import Url exposing (Url)
import Url.Parser as Url exposing ((</>), Parser)



---- MODEL ----


type alias Word =
    String


type alias FormField =
    Word


type alias WordFromApi =
    { word : String
    , id : Int
    }


type alias Model =
    { navKey : Nav.Key
    , page : Page
    , fetchedWord : Maybe WordFromApi
    , translation : Word
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { navKey = key
      , page = urlToPage url
      , fetchedWord = Nothing
      , translation = ""
      }
    , onLoadOrUpdate url
    )


onLoadOrUpdate : Url.Url -> Cmd Msg
onLoadOrUpdate url =
    let
        page =
            urlToPage url
    in
    case page of
        NotFoundPage ->
            Cmd.none

        WordPage id ->
            fetchWord id



---- UPDATE ----


type Msg
    = NoOp
    | SubmitForm
    | SetTranslation String
    | FetchWord (Result Http.Error WordFromApi)
    | LinkClicked UrlRequest
    | UrlChange Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SetTranslation translation ->
            ( { model | translation = translation }, Cmd.none )

        SubmitForm ->
            ( model, Cmd.none )

        --- Fetching ---
        FetchWord (Ok word) ->
            ( { model | fetchedWord = Just word }, Cmd.none )

        FetchWord (Err _) ->
            ( model, Cmd.none )

        --- Routing ---
        LinkClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Nav.pushUrl model.navKey (Url.toString url)
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChange url ->
            ( { model | page = urlToPage url }
            , onLoadOrUpdate url
            )



---- VIEWS ----


view : Model -> Browser.Document Msg
view model =
    { title = "Translation"
    , body =
        [ h1 [] [ text "Translate the following word" ]
        , case model.page of
            NotFoundPage ->
                text "Not Found"

            WordPage _ ->
                formView model
        ]
    }


formView : Model -> Html Msg
formView model =
    let
        vocabulary =
            case model.fetchedWord of
                Just wordFromApi ->
                    wordFromApi.word

                Nothing ->
                    "Fetching went wrong"
    in
    div []
        [ form [ action "https://httpbin.org/post", method "post" ]
            [ div []
                [ span [] [ text "Enlgish" ]
                , span [] [ text "=>" ]
                , span [] [ text "Spanish" ]
                ]
            , div []
                [ span [] [ text vocabulary ]
                ]
            , div []
                [ input
                    [ type_ "text"
                    , placeholder "Enter your translation here"
                    , value model.translation
                    , onInput SetTranslation
                    ]
                    []
                ]
            , button [ type_ "submit" ] [ text "Is it correct?" ]
            ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.application
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        , onUrlChange = UrlChange
        , onUrlRequest = LinkClicked
        }



---- Routing ----


type Page
    = NotFoundPage
    | WordPage Int


urlToPage : Url -> Page
urlToPage url =
    -- We start with our URL
    url
        -- Send it through our URL parser (located below)
        |> Url.parse urlParser
        -- And if it didn't match any known pages, return Index
        |> Maybe.withDefault NotFoundPage



-- The type signature here is a bit gnarly, but you can read it as "a parser for a Page"


urlParser : Parser (Page -> a) a
urlParser =
    -- We try to match one of the following URLs
    Url.oneOf
        -- Url.top matches root (i.e. there is nothing after 'https://example.com')
        [ Url.map NotFoundPage Url.top

        -- Again, Url.s matches a string. </> matches a separator ('/') in the URL, and Url.int matches any integer and "returns" it, so that the user page value gets the user ID
        , Url.map WordPage (Url.s "word" </> Url.int)
        ]



---- DATA FETCHING ----


fetchWord : Int -> Cmd Msg
fetchWord id =
    Http.get
        { url = "http://localhost:3000/vocabularies/" ++ String.fromInt id
        , expect = Http.expectJson FetchWord decodeWord
        }



---- DECODER ----


decodeWord : Decoder WordFromApi
decodeWord =
    Decode.succeed WordFromApi
        |> required "word" string
        |> required "id" int
