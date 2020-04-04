module Main exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Html exposing (Html, a, button, div, form, h1, input, span, text)
import Html.Attributes exposing (href, placeholder, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Http exposing (get)
import Json.Decode as Decode exposing (Decoder, bool, int, string)
import Json.Decode.Pipeline exposing (required)
import Url exposing (Url)
import Url.Builder
import Url.Parser as Url exposing ((</>), Parser)



---- MODEL ----


backendUrl : String
backendUrl =
    -- "http://localhost:3000" -- For testing with json-server
    "http://localhost:8080"


type alias Word =
    { word : String
    , id : Int
    }


type alias Validation =
    { text : String
    , correct : Bool
    }


type alias Model =
    { navKey : Nav.Key
    , page : Page
    , fetchedWord : Maybe Word
    , validation : Maybe Validation
    , translation : String
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { navKey = key
      , page = urlToPage url
      , fetchedWord = Nothing
      , validation = Nothing
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
        WordPage id ->
            getWord id

        _ ->
            Cmd.none



---- UPDATE ----


type Msg
    = NoOp
    | SubmitForm
    | SetTranslation String
    | GetWord (Result Http.Error Word)
    | GetValidation (Result Http.Error Validation)
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
            ( model, validateWord model.translation model.fetchedWord )

        --- Fetching ---
        GetWord (Ok word) ->
            ( { model | fetchedWord = Just word }, Cmd.none )

        GetWord (Err _) ->
            ( { model | fetchedWord = Nothing }, Cmd.none )

        GetValidation (Ok validation) ->
            ( { model | validation = Just validation }, Nav.pushUrl model.navKey "/result" )

        GetValidation (Err _) ->
            ( { model | validation = Nothing }, Cmd.none )

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
        , div []
            [ span [] [ text "Enlgish" ]
            , span [] [ text "=>" ]
            , span [] [ text "Spanish" ]
            ]
        , case model.page of
            NotFoundPage ->
                text "Not Found"

            WordPage _ ->
                formView model

            ResultPage ->
                resultView model
        ]
    }


formView : Model -> Html Msg
formView model =
    case model.fetchedWord of
        Just fetchedWord ->
            div []
                [ form [ onSubmit SubmitForm ]
                    [ div []
                        [ span [] [ text fetchedWord.word ]
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

        Nothing ->
            div [] [ text "Fetching of the word went wrong" ]


resultView : Model -> Html Msg
resultView model =
    case ( model.validation, model.fetchedWord ) of
        ( Just validation, Just fetchedWord ) ->
            div []
                [ div []
                    [ span [] [ text fetchedWord.word ]
                    , span [] [ text " == " ]
                    , span [] [ text model.translation ]
                    ]
                , if validation.correct == True then
                    div [] [ text "your translation was correct" ]

                  else
                    div []
                        [ div [] [ text "your translation was wrong" ]
                        , div [] [ text (validation.text ++ " would have been correct") ]
                        ]
                , a [ href "/word/1" ] [ text "Previous" ]
                , a [ href "/word/2" ] [ text "Next" ]
                ]

        ( _, _ ) ->
            div [] [ text "Fetching went wrong" ]



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
    | ResultPage


urlToPage : Url -> Page
urlToPage url =
    url
        |> Url.parse urlParser
        |> Maybe.withDefault NotFoundPage


urlParser : Parser (Page -> a) a
urlParser =
    Url.oneOf
        [ Url.map NotFoundPage Url.top

        -- Again, Url.s matches a string. </> matches a separator ('/') in the URL, and Url.int matches any integer and "returns" it, so that the user page value gets the user ID
        , Url.map WordPage (Url.s "word" </> Url.int)
        , Url.map ResultPage (Url.s "result")
        ]



---- DATA FETCHING ----


getWord : Int -> Cmd Msg
getWord id =
    let
        url =
            Url.Builder.crossOrigin backendUrl
                [ "vocabularies" ]
                [ Url.Builder.string "id" (String.fromInt id)
                , Url.Builder.string "lang" "es"
                ]

        -- /vocabularies?lang="en"&id=1 or /vocabularies?lang="en"
    in
    Http.get
        { url = url
        , expect = Http.expectJson GetWord decodeWord
        }


validateWord : String -> Maybe Word -> Cmd Msg
validateWord input word =
    case word of
        Just value ->
            let
                url =
                    Url.Builder.crossOrigin backendUrl
                        [ "validation" ]
                        [ Url.Builder.string "lang" "es"
                        , Url.Builder.string "input" input
                        , Url.Builder.string "word-id" (String.fromInt value.id)
                        ]

                -- /vocabularies?lang="en" or /vocabularies/{id}?lang="en"
            in
            Http.get
                { url = url
                , expect = Http.expectJson GetValidation decodeValidation
                }

        Nothing ->
            Cmd.none



---- DECODER ----


decodeWord : Decoder Word
decodeWord =
    Decode.succeed Word
        |> required "word" string
        |> required "id" int


decodeValidation : Decoder Validation
decodeValidation =
    Decode.succeed Validation
        |> required "text" string
        |> required "correct" bool
