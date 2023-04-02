module Pages.Home_ exposing (Model, Msg, page)

import AssocSet
import Browser.Events
import Effect exposing (Effect)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events
import Json.Decode
import Page exposing (Page)
import Random
import Route exposing (Route)
import Shared
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view shared
        }



-- INIT


type alias Model =
    { screen : Screen
    }


type Screen
    = MainMenu
    | InGame Game


type alias Game =
    { score : Int
    , keys : AssocSet.Set Key
    , coin : Maybe Coin
    , mines : List Mine
    , player : Player
    , isGameOver : Bool
    }


type alias Coin =
    { x : Float
    , y : Float
    , kind : CoinKind
    }


type alias Mine =
    { x : Float
    , y : Float
    }


type CoinKind
    = Copper
    | Silver
    | Gold
    | Platinum
    | Diamond


mineGenerator : Player -> Random.Generator Mine
mineGenerator player =
    Random.map2 Mine
        (Random.float 0 1)
        (Random.float 0 1)


coinGenerator : Player -> Random.Generator Coin
coinGenerator player =
    Random.map3 Coin
        (Random.float 0 1)
        (Random.float 0 1)
        coinKindGenerator


coinKindGenerator : Random.Generator CoinKind
coinKindGenerator =
    Random.weighted
        ( 50, Copper )
        [ ( 10, Silver )
        , ( 5, Gold )
        , ( 3, Platinum )
        , ( 1, Diamond )
        ]


coinToValue : Coin -> Int
coinToValue coin =
    case coin.kind of
        Copper ->
            10

        Silver ->
            25

        Gold ->
            50

        Platinum ->
            100

        Diamond ->
            500


type alias Player =
    { x : Float
    , y : Float
    , v : Float
    , rotation : Float
    }


initNewGame : ( Game, Effect Msg )
initNewGame =
    let
        player : Player
        player =
            { x = 0.5
            , y = 0.5
            , rotation = 0
            , v = 0
            }
    in
    ( { score = 0
      , keys = AssocSet.empty
      , coin = Nothing
      , mines = []
      , player = player
      , isGameOver = False
      }
    , coinGenerator player
        |> Random.generate GeneratedCoin
        |> Effect.sendCmd
    )


init : () -> ( Model, Effect Msg )
init () =
    ( { screen = MainMenu }
    , Effect.none
    )



-- UPDATE


type Msg
    = ClickedStartGame
    | ClickedEndGame
    | DismissedGameOverScreen
    | KeyDown Key
    | KeyUp Key
    | Frame Float
    | GeneratedCoin Coin
    | GeneratedMine Mine


type Key
    = ArrowUp
    | ArrowDown
    | ArrowLeft
    | ArrowRight


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case ( msg, model.screen ) of
        ( ClickedStartGame, _ ) ->
            let
                ( game, effect ) =
                    initNewGame
            in
            ( { model | screen = InGame game }
            , effect
            )

        ( ClickedEndGame, MainMenu ) ->
            ( model, Effect.none )

        ( ClickedEndGame, InGame game ) ->
            ( { model
                | screen = InGame { game | isGameOver = True }
              }
            , Effect.none
            )

        ( DismissedGameOverScreen, MainMenu ) ->
            ( model, Effect.none )

        ( DismissedGameOverScreen, InGame game ) ->
            ( { model | screen = MainMenu }
            , Effect.saveHighscore game.score
            )

        ( KeyUp key, MainMenu ) ->
            ( model, Effect.none )

        ( KeyUp key, InGame game ) ->
            ( { model
                | screen = InGame { game | keys = AssocSet.remove key game.keys }
              }
            , Effect.none
            )

        ( KeyDown key, MainMenu ) ->
            ( model, Effect.none )

        ( KeyDown key, InGame game ) ->
            ( { model
                | screen = InGame { game | keys = AssocSet.insert key game.keys }
              }
            , Effect.none
            )

        ( Frame _, MainMenu ) ->
            ( model
            , Effect.none
            )

        ( Frame msElapsed, InGame game ) ->
            let
                ( newGame, effect ) =
                    game
                        |> updatePlayer msElapsed
                        |> updateMines msElapsed
                        |> checkIfPlayerExploded
                        |> checkIfPickingUpCoin
            in
            ( { model | screen = InGame newGame }
            , effect
            )

        ( GeneratedCoin _, MainMenu ) ->
            ( model, Effect.none )

        ( GeneratedCoin coin, InGame game ) ->
            ( { model | screen = InGame { game | coin = Just coin } }
            , Effect.none
            )

        ( GeneratedMine _, MainMenu ) ->
            ( model, Effect.none )

        ( GeneratedMine mine, InGame game ) ->
            ( { model | screen = InGame { game | mines = mine :: game.mines } }
            , Effect.none
            )


updateMines : Float -> Game -> Game
updateMines msElapsed game =
    let
        moveMine : Mine -> Mine
        moveMine mine =
            let
                d =
                    getDistance mine game.player

                speed =
                    0.0001
            in
            mine
    in
    { game | mines = List.map moveMine game.mines }


updatePlayer : Float -> Game -> Game
updatePlayer msElapsed game =
    let
        player : Player
        player =
            game.player

        movement : Movement
        movement =
            game.keys
                |> AssocSet.foldl updateMovement { move = 0, turn = 0 }

        newPlayer : Player
        newPlayer =
            player
                |> applyMovement msElapsed movement
    in
    { game | player = newPlayer }


checkIfPlayerExploded : Game -> Game
checkIfPlayerExploded game =
    if List.any (isIntersecting game.player) game.mines then
        { game | isGameOver = True }

    else
        game


checkIfPickingUpCoin : Game -> ( Game, Effect Msg )
checkIfPickingUpCoin game =
    case game.coin of
        Just coin ->
            if isIntersecting coin game.player then
                ( { game | coin = Nothing, score = game.score + coinToValue coin }
                , Effect.batch
                    [ coinGenerator game.player
                        |> Random.generate GeneratedCoin
                        |> Effect.sendCmd
                    , mineGenerator game.player
                        |> Random.generate GeneratedMine
                        |> Effect.sendCmd
                    ]
                )

            else
                ( game
                , Effect.none
                )

        Nothing ->
            ( game, Effect.none )


isIntersecting : { a | x : Float, y : Float } -> { b | x : Float, y : Float } -> Bool
isIntersecting a b =
    let
        radius =
            1 / 38
    in
    getDistance a b < radius + radius


getDistance : { a | x : Float, y : Float } -> { b | x : Float, y : Float } -> Float
getDistance a b =
    sqrt ((a.x - b.x) * (a.x - b.x) + ((a.y - b.y) * (a.y - b.y)))


getAngleInRadians : { a | x : Float, y : Float } -> { b | x : Float, y : Float } -> Float
getAngleInRadians a b =
    let
        slope =
            atan2 (b.y - a.y) (b.x - a.x)
    in
    slope


type alias Movement =
    { move : Float
    , turn : Float
    }


acceleration =
    0.000001


rotation_rate =
    pi / 64


friction =
    0.0000004


minPosition =
    0


maxPosition =
    1


maxVelocity =
    0.00035


updateMovement : Key -> Movement -> Movement
updateMovement key movement =
    case key of
        ArrowUp ->
            { movement | move = movement.move + acceleration }

        ArrowDown ->
            { movement | move = movement.move - acceleration }

        ArrowLeft ->
            { movement | turn = movement.turn - rotation_rate }

        ArrowRight ->
            { movement | turn = movement.turn + rotation_rate }


applyMovement : Float -> Movement -> Player -> Player
applyMovement dt { move, turn } player =
    let
        dv : Float
        dv =
            move
                + (if player.v > 0 then
                    -friction

                   else if player.v < 0 then
                    friction

                   else
                    0
                  )

        newRotation : Float
        newRotation =
            player.rotation + turn

        newV : Float
        newV =
            (dv * dt + player.v)
                |> Basics.clamp -maxVelocity maxVelocity

        newX : Float
        newX =
            (cos player.rotation * newV * dt + player.x)
                |> Basics.clamp minPosition maxPosition

        newY : Float
        newY =
            (sin player.rotation * newV * dt + player.y)
                |> Basics.clamp minPosition maxPosition
    in
    { player
        | v = newV
        , x = newX
        , y = newY
        , rotation = newRotation
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.screen of
        MainMenu ->
            Sub.none

        InGame game ->
            if game.isGameOver then
                Sub.none

            else
                Sub.batch
                    [ Browser.Events.onAnimationFrameDelta Frame
                    , Browser.Events.onKeyDown (keyEventDecoder KeyDown)
                    , Browser.Events.onKeyUp (keyEventDecoder KeyUp)
                    ]


keyEventDecoder : (Key -> Msg) -> Json.Decode.Decoder Msg
keyEventDecoder toMsg =
    let
        toKey : String -> Json.Decode.Decoder Key
        toKey code =
            case code of
                "ArrowUp" ->
                    Json.Decode.succeed ArrowUp

                "ArrowDown" ->
                    Json.Decode.succeed ArrowDown

                "ArrowLeft" ->
                    Json.Decode.succeed ArrowLeft

                "ArrowRight" ->
                    Json.Decode.succeed ArrowRight

                _ ->
                    Json.Decode.fail "Another key was pressed"
    in
    Json.Decode.field "code" Json.Decode.string
        |> Json.Decode.andThen toKey
        |> Json.Decode.map toMsg



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = "Dubloon Disaster"
    , body =
        case model.screen of
            MainMenu ->
                [ h1 [] [ text "Dubloon Disaster" ]
                , button
                    [ Html.Events.onClick ClickedStartGame ]
                    [ text "Start game" ]
                , case List.maximum shared.highscores of
                    Just highscore ->
                        p [] [ text ("Highscore: " ++ String.fromInt highscore) ]

                    Nothing ->
                        text ""
                ]

            InGame game ->
                let
                    viewCoin =
                        case game.coin of
                            Just coin ->
                                div
                                    [ class "coin"
                                    , style "transform"
                                        ("translate(calc(${x}rem - 50%), calc(${y}rem - 50%)"
                                            |> String.replace "${x}" (String.fromFloat (toRem coin.x))
                                            |> String.replace "${y}" (String.fromFloat (toRem coin.y))
                                        )
                                    ]
                                    [ coinToValue coin
                                        |> String.fromInt
                                        |> text
                                    ]

                            Nothing ->
                                text ""

                    viewPlayerBoat =
                        div
                            [ class "player"
                            , style "transform"
                                ("translate(calc(${x}rem - 50%), calc(${y}rem - 50%)"
                                    |> String.replace "${x}" (String.fromFloat (toRem game.player.x))
                                    |> String.replace "${y}" (String.fromFloat (toRem game.player.y))
                                )
                            ]
                            [ div
                                [ class "arrow"
                                , style "transform"
                                    ("rotate(${r}rad)"
                                        |> String.replace "${r}" (String.fromFloat game.player.rotation)
                                    )
                                ]
                                []
                            ]

                    viewMine : Mine -> Html Msg
                    viewMine mine =
                        div
                            [ class "mine"
                            , style "transform"
                                ("translate(calc(${x}rem - 50%), calc(${y}rem - 50%)"
                                    |> String.replace "${x}" (String.fromFloat (toRem mine.x))
                                    |> String.replace "${y}" (String.fromFloat (toRem mine.y))
                                )
                            ]
                            [ div
                                [ class "arrow"
                                , style "transform"
                                    ("rotate(${r}rad)"
                                        |> String.replace "${r}" (String.fromFloat (getAngleInRadians mine game.player))
                                    )
                                ]
                                []
                            ]
                in
                [ div [ class "game" ]
                    [ div [ class "score" ]
                        [ text ("Score: " ++ String.fromInt game.score) ]
                    , viewCoin
                    , viewPlayerBoat
                    , div []
                        (List.map viewMine game.mines)
                    , button
                        [ class "end-game"
                        , Html.Events.onClick ClickedEndGame
                        ]
                        [ text "End game" ]
                    , if game.isGameOver then
                        div [ class "dialog" ]
                            [ h3 [] [ text "Game over" ]
                            , p []
                                [ text ("Score: " ++ String.fromInt game.score)
                                ]
                            , button
                                [ Html.Events.onClick DismissedGameOverScreen ]
                                [ text "Save this score" ]
                            ]

                      else
                        text ""
                    ]
                ]
    }


toRem : Float -> Float
toRem percent =
    let
        min =
            1

        max =
            39
    in
    (max - min) * percent + min
