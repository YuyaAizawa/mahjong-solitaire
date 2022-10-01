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

type alias Coords = ( Int, Int, Int ) -- a pai occupies 2x2x1 Coords

type Pai = Pai Char

init : () -> ( Model, Cmd Msg )
init _ =
  ( { board = Dict.empty
    , hold = Nothing
    }
  , Random.generate PileUp (pileUp standerdMold allPais)
  )

standerdMold : List Coords
standerdMold =
  standerdMoldString
    |> List.indexedMap (\z listStr -> ( z, listStr ))
    |> List.concatMap (\( z, listStr ) ->
        listStr
          |> List.indexedMap (\y str -> ( y, str ))
          |> List.concatMap (\( y, str ) ->
              str
                |> String.toList
                |> List.indexedMap (\x char -> ( ( x, y, z ), char ))))
    |> List.filter (\( _, char ) -> char == 'o')
    |> List.map Tuple.first



-- UPDATE --

type Msg
  = PaiClicked Int Int Int
  | PileUp (Dict Coords Pai)

update : Msg -> Model -> (Model, Cmd Msg)
update msg { board, hold } =
  case msg of
    PaiClicked x y z ->
      let
        newModel =
          if board |> isBlocked x y z
          then
            Model board hold
          else
            case hold of

              Nothing ->
                Model board (Just ( x, y, z ))

              Just ( hx, hy, hz ) ->
                if ( hx, hy, hz ) == ( x, y, z )
                then
                  Model board Nothing
                else if Dict.get ( hx, hy, hz ) board == Dict.get ( x, y, z ) board
                then
                  let
                    newBoard =
                      board
                        |> Dict.remove ( hx, hy, hz )
                        |> Dict.remove ( x, y, z )
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

isBlocked : Int -> Int -> Int -> Dict Coords Pai -> Bool
isBlocked x y z board =
  isSandwiched x y z board || isRidden x y z board

isSandwiched : Int -> Int -> Int -> Dict Coords Pai -> Bool
isSandwiched x y z board =
  let
    left  = board |> Dict.get ( x - 2, y, z )
    right = board |> Dict.get ( x + 2, y, z )
  in
    case ( left, right ) of
      ( Just _, Just _ ) -> True
      _                  -> False

isRidden : Int -> Int -> Int -> Dict Coords Pai -> Bool
isRidden x y z board =
  List.range -1 1
    |> List.concatMap (\dy ->
        List.range -1 1
          |> List.map (\dx -> ( x + dx, y + dy, z + 1)))
    |> List.any (\coords -> Dict.member coords board)

isMatch : Pai -> Pai -> Bool
isMatch (Pai char1) (Pai char2) =
  if List.member char1 huapaiChars then
    List.member char2 huapaiChars
  else if List.member char1 sijipaiChars then
    List.member char2 sijipaiChars
  else
    char1 == char2



-- VIEW --

view : Model -> Html Msg
view model =
  div [ id "elm-area" ]
    [ Svg.svg
        [ SAttr.width "856"
        , SAttr.height "564"
        , SAttr.viewBox <| "-10 -10 750 550"
        ]
        [ boardView model ]
    ]

boardView : Model -> Svg Msg
boardView { board, hold } =
  let
    pais =
      board
        |> Dict.toList
        |> List.sortBy (\( ( _, _, z ), _ ) -> z)
        |> List.map (\( ( x, y, z ), pai ) -> tileView x y z pai )

    selected =
      [()]
        |> List.filterMap (\_ -> hold)
        |> List.map (\( x, y, z ) -> holdView x y z)
  in
    Svg.g []
      [ Svg.g [ SAttr.class "board" ] pais
      , Svg.g [ SAttr.class "selected" ] selected
      ]

tileView : Int -> Int -> Int -> Pai -> Svg Msg
tileView x y z (Pai char) =
  Svg.g
    [ SAttr.class "tile"
    , translate x y z
    ]
    [ Svg.rect
        [ SAttr.x "4"
        , SAttr.y "8"
        , SAttr.rx "5"
        , SAttr.ry "5"
        , SAttr.width "48"
        , SAttr.height "63"
        , SAttr.fill "#E5CA80"
        , onClick <| PaiClicked x y z
        ] []
    , Svg.rect
        [ SAttr.x "2"
        , SAttr.y "5"
        , SAttr.rx "5"
        , SAttr.ry "5"
        , SAttr.width "48"
        , SAttr.height "63"
        , SAttr.fill "#FDF9EE"
        , onClick <| PaiClicked x y z
        ] []
    , Svg.rect
        [ SAttr.x "0"
        , SAttr.y "0"
        , SAttr.rx "5"
        , SAttr.ry "5"
        , SAttr.width "48"
        , SAttr.height "63"
        , SAttr.fill "#FDF9EE"
        , onClick <| PaiClicked x y z
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

holdView : Int -> Int -> Int -> Svg Msg
holdView x y z =
  Svg.g
    [ translate x y z
    , onClick <| PaiClicked x y z
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

translate : Int -> Int -> Int -> Svg.Attribute msg
translate x y z =
  SAttr.transform <| "translate("
      ++ String.fromInt (x * 24 - z * 4) ++ " "
      ++ String.fromInt (y * 32 - z * 8) ++ ")"


-- SUBSCRIPTION --

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



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



-- PAI --

allPais : List Pai
allPais =
  List.concat
    [ wanziChars   |> List.concatMap (List.repeat 4)
    , suoziChars   |> List.concatMap (List.repeat 4)
    , tongziChars  |> List.concatMap (List.repeat 4)
    , zipaiChars   |> List.concatMap (List.repeat 4)
    , huapaiChars
    , sijipaiChars
    ]
    |> List.map Pai

wanziChars : List Char
wanziChars =
  List.range 0 8
    |> List.map (\i -> i + 0x1F007) -- '\u{1F007}' = '🀇'
    |> List.map Char.fromCode

suoziChars : List Char
suoziChars =
  List.range 0 8
    |> List.map (\i -> i + 0x1F010) -- '\u{1F010}' = '🀐'
    |> List.map Char.fromCode

tongziChars : List Char
tongziChars =
  List.range 0 8
    |> List.map (\i -> i + 0x1F019) -- '\u{1F019}' = '🀙'
    |> List.map Char.fromCode

zipaiChars : List Char
zipaiChars =
  List.range 0 6
    |> List.map (\i -> i + 0x1F000) -- '\u{1F000}' = '🀀'
    |> List.map Char.fromCode

huapaiChars : List Char
huapaiChars =
  List.range 0 3
    |> List.map (\i -> i + 0x1F022) -- '\u{1F022}' = '🀢'
    |> List.map Char.fromCode

sijipaiChars : List Char
sijipaiChars =
  List.range 0 3
    |> List.map (\i -> i + 0x1F026) -- '\u{1F026}' = '🀦'
    |> List.map Char.fromCode



-- MOLD --

standerdMoldString : List (List String)
standerdMoldString =
  [ [ "  o o o o o o o o o o o o     "
    , "                              "
    , "      o o o o o o o o         "
    , "                              "
    , "    o o o o o o o o o o       "
    , "                              "
    , "  o o o o o o o o o o o o     "
    , "o                         o o "
    , "  o o o o o o o o o o o o     "
    , "                              "
    , "    o o o o o o o o o o       "
    , "                              "
    , "      o o o o o o o o         "
    , "                              "
    , "  o o o o o o o o o o o o     "
    , "                              "
    ]
  , [ "                              "
    , "                              "
    , "        o o o o o o           "
    , "                              "
    , "        o o o o o o           "
    , "                              "
    , "        o o o o o o           "
    , "                              "
    , "        o o o o o o           "
    , "                              "
    , "        o o o o o o           "
    , "                              "
    , "        o o o o o o           "
    , "                              "
    , "                              "
    , "                              "
    ]
  , [ "                              "
    , "                              "
    , "                              "
    , "                              "
    , "          o o o o             "
    , "                              "
    , "          o o o o             "
    , "                              "
    , "          o o o o             "
    , "                              "
    , "          o o o o             "
    , "                              "
    , "                              "
    , "                              "
    , "                              "
    , "                              "
    ]
  , [ "                              "
    , "                              "
    , "                              "
    , "                              "
    , "                              "
    , "                              "
    , "            o o               "
    , "                              "
    , "            o o               "
    , "                              "
    , "                              "
    , "                              "
    , "                              "
    , "                              "
    , "                              "
    , "                              "
    ]
  , [ "                              "
    , "                              "
    , "                              "
    , "                              "
    , "                              "
    , "                              "
    , "                              "
    , "             o                "
    , "                              "
    , "                              "
    , "                              "
    , "                              "
    , "                              "
    , "                              "
    , "                              "
    , "                              "
    ]
  ]