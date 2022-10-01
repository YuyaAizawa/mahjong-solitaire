module Main exposing (main)

import Array exposing (Array)
import Browser
import Dict exposing (Dict)
import Html exposing (Html, div, text, button)
import Html.Attributes as HAttr exposing (id, class)
import Html.Events exposing (onClick)
import Random exposing (Generator)
import Svg exposing (Svg)
import Svg.Attributes as SAttr



main =
  Browser.element
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }



-- MODEL --

type alias Model =
  { board : Dict Coords Pai
  , hold : Maybe Coords
  }

type alias Coords = ( Int, Int )

type Pai = Pai Char

init : () -> ( Model, Cmd Msg )
init _ =
  ( { board = Dict.empty
    , hold = Nothing
    }
  , Random.generate PileUp (pileUp simpleMold allPais)
  )

simpleMold : List Coords
simpleMold =
  List.range 1 4
    |> List.concatMap (\y ->
        List.range 1 9
          |> List.map (\x -> ( x, y )))

allPais : List Pai
allPais =
  List.range 1 9
    |> List.concatMap (\n -> List.repeat 4 (tongzi n))



-- UPDATE --

type Msg
  = PaiClicked Int Int
  | PileUp (Dict Coords Pai)

update : Msg -> Model -> (Model, Cmd Msg)
update msg { board, hold } =
  case msg of
    PaiClicked x y ->
      let
        newModel =
          if board |> isBlocked x y
          then
            Model board hold
          else
            case hold of

              Nothing ->
                Model board (Just ( x, y ))

              Just ( hx, hy ) ->
                if ( hx, hy ) == ( x, y )
                then
                  Model board Nothing
                else if Dict.get ( hx, hy ) board == Dict.get ( x, y ) board
                then
                  let
                    newBoard =
                      board
                        |> Dict.remove ( hx, hy )
                        |> Dict.remove (x, y)
                  in
                    Model newBoard Nothing
                else
                  Model board hold
      in
        ( newModel, Cmd.none )

    PileUp newBoard ->
      ( Model newBoard Nothing
      , Cmd.none
      )

isBlocked : Int -> Int -> Dict Coords Pai -> Bool
isBlocked x y board =
  let
    left  = board |> Dict.get ( x - 1, y )
    right = board |> Dict.get ( x + 1, y )
  in
    case ( left, right ) of
      ( Just _, Just _ ) -> True
      _                  -> False



-- VIEW --

viewBoxWidth  = "560"
viewBoxHeight = "360"

view : Model -> Html Msg
view model =
  div [ id "elm-area" ]
    [ Svg.svg
        [ SAttr.width viewBoxWidth
        , SAttr.height viewBoxHeight
        , SAttr.viewBox <| "0 0 "++viewBoxWidth++" "++viewBoxHeight
        ]
        [ boardView model ]
    ]

boardView : Model -> Svg Msg
boardView { board, hold } =
  let
    pais =
      board
        |> Dict.toList
        |> List.map (\( ( x, y ), pai ) -> tileView x y pai )

    selected =
      [()]
        |> List.filterMap (\_ -> hold)
        |> List.map (\( x, y ) -> holdView x y)
  in
    Svg.g []
      [ Svg.g [ SAttr.class "board" ] pais
      , Svg.g [ SAttr.class "selected" ] selected
      ]

tileView : Int -> Int -> Pai -> Svg Msg
tileView x y (Pai char) =
  Svg.g
    [ SAttr.class "tile"
    , SAttr.transform <| "translate("
      ++ String.fromInt (x * 48) ++ " "
      ++ String.fromInt (y * 64) ++ ")"
    ]
    [ Svg.rect
        [ SAttr.x "0"
        , SAttr.y "0"
        , SAttr.rx "5"
        , SAttr.ry "5"
        , SAttr.width "48"
        , SAttr.height "63"
        , SAttr.fill "#FDF9EE"
        , onClick <| PaiClicked x y
        ] []
    , Svg.text_
        [ SAttr.fontSize "90"
        , SAttr.x "-3"
        , SAttr.y "62"
        , SAttr.fill "#333"
        , SAttr.pointerEvents "none"
        ]
        [ Svg.text <| String.fromChar <| char ]
    , Svg.rect
        [ SAttr.x "0.3"
        , SAttr.y "0.3"
        , SAttr.rx "5"
        , SAttr.ry "5"
        , SAttr.width "47.5"
        , SAttr.height "63.5"
        , SAttr.stroke "#222"
        , SAttr.strokeWidth "1.5"
        , SAttr.fill "none"
        ] []
    ]

holdView : Int -> Int -> Svg Msg
holdView x y =
  Svg.g
    [ SAttr.transform <| "translate("
      ++ String.fromInt (x * 48) ++ " "
      ++ String.fromInt (y * 64) ++ ")"
    , onClick <| PaiClicked x y
    ]
    [ Svg.rect
        [ SAttr.x "0.3"
        , SAttr.y "0.3"
        , SAttr.rx "5"
        , SAttr.ry "5"
        , SAttr.width "47.5"
        , SAttr.height "63.5"
        , SAttr.stroke "#2EE"
        , SAttr.strokeWidth "2"
        , SAttr.fill "none"
        ]
        []
    ]



-- SUBSCRIPTION --

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- PAI --

wanzi : Int -> Pai
wanzi num =
  wanziArray
    |> Array.get (num - 1)
    |> Maybe.withDefault '\u{0000}'
    |> Pai

suozi : Int -> Pai
suozi num =
  suoziArray
    |> Array.get (num - 1)
    |> Maybe.withDefault '\u{0000}'
    |> Pai

tongzi : Int -> Pai
tongzi num =
  tongziArray
    |> Array.get (num - 1)
    |> Maybe.withDefault '\u{0000}'
    |> Pai

wanziArray : Array Char
wanziArray =
  List.range 0 8
    |> List.map (\i -> i + 0x1F007) -- '\u{1F007}' = '🀇'
    |> List.map Char.fromCode
    |> Array.fromList

suoziArray : Array Char
suoziArray =
  List.range 0 8
    |> List.map (\i -> i + 0x1F010) -- '\u{1F010}' = '🀐'
    |> List.map Char.fromCode
    |> Array.fromList

tongziArray : Array Char
tongziArray =
  List.range 0 8
    |> List.map (\i -> i + 0x1F019) -- '\u{1F019}' = '🀙'
    |> List.map Char.fromCode
    |> Array.fromList



-- SHUFFLE --

jalajala : List Pai -> Generator (List Pai)
jalajala pais =
  Random.independentSeed
    |> Random.map (\initSeed ->
        pais
          |> List.foldl
              (\pai ( list, seed ) ->
                  let
                    ( rank, nextSeed ) =
                      seed |> Random.step anyInt -- ignore minor biases
                  in
                    ( ( pai, rank ) :: list, nextSeed ))
              ( [], initSeed )
          |> (\( list, _ ) -> list)
          |> List.sortBy (\( _, rank ) -> rank)
          |> List.map (\( pai, _ ) -> pai))

anyInt : Generator Int
anyInt =
  Random.int Random.minInt Random.maxInt

pileUp : List Coords -> List Pai -> Generator (Dict Coords Pai)
pileUp mold pais =
  jalajala pais
    |> Random.andThen (\shuffled ->
        let
          board =
            List.map2 Tuple.pair mold shuffled
              |> Dict.fromList
        in
          Random.constant board)
