import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (value, type_, id, class, for)
import Html.Events exposing (..)
import Http exposing (..)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E exposing (Value)

main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

type Model
    = Login String String
    | LoggingIn
    | GettingUserData String String
    | LoggedIn String String String User
    | UpdatingData String String User
    | Error (Maybe Http.Error)

type Msg
    = LoginName String
    | LoginPassword String
    | SubmitLogin
    | GotLogin (Result Http.Error (String, String))
    | GotUserData (Result UpdateError User)
    | SetField FormField String
    | UpdateData

type UpdateError
    = ValidationError String
    | HttpError Http.Error

type alias User =
    { firstName : String
    , lastName : String
    , address : Address
    }

type alias Address =
    { countryCode : String
    , zipCode : String
    , streetAddress : String
    , streetName : String
    , houseNo : String
    , staircase : String
    , apartment : String
    }

type FormField
    = CountryCode
    | ZipCode
    | StreetAddress
    | StreetName
    | HouseNo
    | Staircase
    | Apartment

init : () -> (Model, Cmd Msg)
init _ =
    ( Login "" ""
    , Cmd.none
    )

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

-- Update

updateAddress : FormField -> String -> Address -> Address
updateAddress field value addr =
    case field of
        CountryCode ->
            { addr | countryCode = value }
        ZipCode ->
            { addr | zipCode = value }
        StreetAddress ->
            { addr | streetAddress = value }
        StreetName ->
            { addr | streetName = value }
        HouseNo ->
            { addr | houseNo = value }
        Staircase ->
            { addr | staircase = value }
        Apartment ->
            { addr | apartment = value }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( Login _ pwd, LoginName eml ) ->
            ( Login eml pwd, Cmd.none )

        ( Login eml _, LoginPassword pwd ) ->
            ( Login eml pwd, Cmd.none )

        ( Login eml pwd, SubmitLogin ) ->
            ( LoggingIn, Http.post
                  { url = "https://persona.api.ksfmedia.fi/v1/login"
                  , body = loginJson eml pwd
                  , expect = Http.expectJson GotLogin loginDecoder
                  })

        ( _, GotLogin res ) ->
            case res of
                Ok ( uuid, token ) ->
                    ( GettingUserData uuid token, Http.request
                          { method = "GET"
                          , headers = [ Http.header "Authorization" ("OAuth " ++ token) ]
                          , url = "https://persona.api.ksfmedia.fi/v1/users/" ++ uuid
                          , body = Http.emptyBody
                          , expect = Http.expectJson (\result -> case result of
                                                                  Ok x -> GotUserData (Ok x)
                                                                  Err x -> GotUserData (Err (HttpError x))
                                                     ) userDataDecoder
                          , timeout = Nothing
                          , tracker = Nothing
                          })
                Err err ->
                    ( Error (Just err), Cmd.none )

        ( GettingUserData uuid token, GotUserData res ) ->
            case res of
                Ok data ->
                    ( LoggedIn uuid token "" data, Cmd.none )
                Err (HttpError err) ->
                    ( Error (Just err), Cmd.none )
                _ ->
                    ( Error Nothing, Cmd.none )

        ( LoggedIn uuid token _ data, SetField field value ) ->
            ( LoggedIn uuid token ""
                  { data | address = updateAddress field value data.address }
            , Cmd.none )

        ( LoggedIn uuid token _ data, UpdateData ) ->
            ( UpdatingData uuid token data, Http.request
                  { method = "PATCH"
                  , headers = [ Http.header "Authorization" ("OAuth " ++ token) ]
                  , url = "https://persona.api.ksfmedia.fi/v1/users/" ++ uuid
                  , body = userDataJson data
                  , expect = expectUpdatedData
                  , timeout = Nothing
                  , tracker = Nothing
                  } )

        ( UpdatingData uuid token oldData, GotUserData res ) ->
            case res of
                Ok data ->
                    ( LoggedIn uuid token "" data, Cmd.none )
                Err (ValidationError err) ->
                    ( LoggedIn uuid token err oldData, Cmd.none )
                Err (HttpError err) ->
                    ( Error (Just err), Cmd.none )

        _ -> ( Error Nothing, Cmd.none )

-- View

viewInput : String -> String -> String -> String -> (String -> msg) -> List (Html msg)
viewInput i t n v toMsg =
    [ span [ class "input" ]
          [ label [ for i ] [ text n ]
          , input [ id i, type_ t, value v, onInput toMsg ] []
          ]
    , br [] []
    ]

errorDocument : String -> Document Msg
errorDocument txt = Document "Error"
                    [ div []
                        [ commonHeading
                        , h2 [] [ text "Error" ]
                        , div [ id "error" ] [ text txt ]
                        ]
                    ]

commonHeading : Html msg
commonHeading = h1 [] [ text "KSF user data demo" ]

view : Model -> Document Msg
view model =
    case model of
        Login usr pwd ->
            Document "Login"
                [ div [ id "login" ]
                      [ commonHeading
                      , h2 [] [ text "Login" ]
                      , form [ onSubmit SubmitLogin ]
                          ( List.concat
                                [ viewInput "user" "email" "Email"
                                      usr LoginName
                                , viewInput "password" "password" "Password"
                                      pwd LoginPassword
                                , [ button [ type_ "submit" ] [ text "Login" ] ]
                                ]
                          )
                      ]
                ]

        LoggingIn ->
            Document "Logging in"
            [ div []
                  [ commonHeading
                  , h2 [] [ text "Please wait" ]
                  , text "Logging in"
                  ]
            ]

        GettingUserData _ _ ->
            Document "Retrieving user data"
            [ div []
                  [ commonHeading
                  , h2 [] [ text "Please wait" ]
                  , text "Retrieving user data"
                  ]
            ]

        LoggedIn _ _ err {firstName, lastName, address} ->
            Document "KSF user data"
            [ div []
                  [ commonHeading
                  , h2 [] [ text "KSF user data" ]
                  , div []
                        [ text ("Name: " ++ firstName ++ " " ++ lastName) ]
                  , h3 [] [ text "Address" ]
                  , form [ onSubmit UpdateData ]
                        ( List.concat
                              [ viewInput "countryCode" "text" "Country Code"
                                    address.countryCode (SetField CountryCode)
                              , viewInput "zipCode" "text" "Zip"
                                    address.zipCode (SetField ZipCode)
                              , viewInput "streetAddress" "text" "Street Address"
                                    address.streetAddress (SetField StreetAddress)
                              , viewInput "streetName" "text" "Street Name"
                                    address.streetName (SetField StreetName)
                              , viewInput "houseNo" "text" "House No"
                                    address.houseNo (SetField HouseNo)
                              , viewInput "staircase" "text" "Staircase"
                                    address.staircase (SetField Staircase)
                              , viewInput "apartment" "text" "Apartment"
                                    address.apartment (SetField Apartment)
                              , [ button [ type_ "submit" ] [ text "Update address" ] ]
                              ]
                        )
                  , div [ id "error" ] [ text err ]
                  ] ]

        UpdatingData _ _ _ ->
            Document "Updating user address"
            [ div []
                  [ commonHeading
                  , h2 [] [ text "Please wait" ]
                  , text "Updating user address"
                  ]
            ]

        Error Nothing ->
            errorDocument "Unexpected program state"
        Error (Just (BadUrl url)) ->
            errorDocument ("Bad url " ++ url)
        Error (Just Timeout) ->
            errorDocument "Timeout"
        Error (Just NetworkError) ->
            errorDocument "Network error"
        Error (Just (BadStatus status)) ->
            errorDocument ("Error status " ++ String.fromInt status)
        Error (Just (BadBody txt)) ->
            errorDocument ("Bad body " ++ txt)

-- Decoders

loginDecoder : Decoder ( String, String )
loginDecoder = D.map2 (Tuple.pair) (D.field "uuid" D.string) (D.field "token" D.string)

maybeEmptyString : Decoder String -> Decoder String
maybeEmptyString decoder = D.oneOf [ decoder
                                   , D.succeed ""
                                   ]

userDataAddress : String -> Decoder String
userDataAddress field = maybeEmptyString <| D.field "address" (D.field field D.string)

userDataDecoder : Decoder User
userDataDecoder = D.map3 User
                  (maybeEmptyString <| D.field "firstName" D.string)
                  (maybeEmptyString <| D.field "lastName" D.string)
                  (D.map7 Address
                       (userDataAddress "countryCode")
                       (userDataAddress "zipCode")
                       (userDataAddress "streetAddress")
                       (userDataAddress "streetName")
                       (userDataAddress "houseNo")
                       (userDataAddress "staircase")
                       (userDataAddress "apartment"))

validationErrorDecoder : Decoder String
validationErrorDecoder = D.field "invalid_request_body" (D.field "message" D.string)

expectUpdatedData : Expect Msg
expectUpdatedData =
    Http.expectStringResponse GotUserData <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (HttpError (Http.BadUrl url))

                Http.Timeout_ ->
                    Err (HttpError Http.Timeout)

                Http.NetworkError_ ->
                    Err (HttpError Http.NetworkError)

                Http.BadStatus_ metadata body ->
                    if metadata.statusCode == 400 then
                        case D.decodeString validationErrorDecoder body of
                            Ok err -> Err (ValidationError err)
                            _ -> Err (HttpError (Http.BadStatus 400))

                    else
                        Err (HttpError (Http.BadStatus metadata.statusCode))

                Http.GoodStatus_ metadata body ->
                    case D.decodeString userDataDecoder body of
                        Ok value ->
                            Ok value

                        Err err ->
                            Err (HttpError (Http.BadBody (D.errorToString err)))

-- Encoders

loginJson : String -> String -> Body
loginJson eml pwd =
    jsonBody <| E.object
        [ ( "username", E.string eml )
        , ( "password", E.string pwd )
        ]

emptyNull : String -> E.Value
emptyNull str = case str of
                    "" -> E.null
                    _ -> E.string str

updateField : String -> String -> List (String, Value)
updateField key value =
    case value of
        "" -> []
        _ -> [ ( key, E.string value ) ]

userDataJson : User -> Body
userDataJson usr =
    jsonBody <| E.object
        [ ( "address",
                E.object <| List.concat
                    [ updateField "countryCode" usr.address.countryCode
                    , updateField "zipCode" usr.address.zipCode
                    , updateField "streetAddress" usr.address.streetAddress
                    , updateField "streetName" usr.address.streetName
                    , updateField "houseNo" usr.address.houseNo
                    , updateField "staircase" usr.address.staircase
                    , updateField "apartment" usr.address.apartment
                    ]
          )
        ]
