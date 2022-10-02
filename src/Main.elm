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
  , hold : Maybe PaiOnBoard
  , history : List ( PaiOnBoard, PaiOnBoard )
  }

type alias Coords = ( Int, Int, Int ) -- a pai occupies 2x2x1 Coords

type Pai = Pai Char ColorOverlay
type alias ColorOverlay = List (Svg Msg)

type alias PaiOnBoard = ( Coords, Pai )

init : () -> ( Model, Cmd Msg )
init _ =
  ( { board = Dict.empty
    , hold = Nothing
    , history = []
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
  = PaiClicked PaiOnBoard
  | Undo
  | PileUp (Dict Coords Pai)

update : Msg -> Model -> (Model, Cmd Msg)
update msg { board, hold, history } =
  case msg of
    PaiClicked ( clickedCoords, clickedPai ) ->
      let
        model_ =
          if isBlocked clickedCoords board
          then
            Model board hold history
          else
            case hold of

              Nothing ->
                Model board (Just ( clickedCoords, clickedPai )) history

              Just ( holdCoords, holdPai ) ->
                if clickedCoords == holdCoords then
                  Model board Nothing history
                else if isMatch holdPai clickedPai then
                  let
                    board_ =
                      board
                        |> Dict.remove holdCoords
                        |> Dict.remove clickedCoords

                    history_ =
                      ( ( holdCoords, holdPai )
                      , ( clickedCoords, clickedPai )
                      ) :: history
                  in
                    Model board_ Nothing history_
                else
                  Model board hold history
      in
        ( model_, Cmd.none )

    Undo ->
      let
        model_ =
          case history of
            [] ->
              Model board Nothing []

            ( ( coords1, pai1 ), ( coords2, pai2 ) ) :: history_ ->
              let
                board_ =
                  board
                    |> Dict.insert coords1 pai1
                    |> Dict.insert coords2 pai2
              in
                Model board_ Nothing history_
      in
        ( model_, Cmd.none )

    PileUp board_ ->
      ( Model board_ Nothing []
      , Cmd.none
      )

isBlocked : Coords -> Dict Coords Pai -> Bool
isBlocked coords board =
  isSandwiched coords board || isRidden coords board

isSandwiched : Coords -> Dict Coords Pai -> Bool
isSandwiched ( x, y, z ) board =
  let
    range = List.range -1 1
    left  = List.any (\dy -> Dict.member ( x - 2, y + dy, z ) board) range
    right = List.any (\dy -> Dict.member ( x + 2, y + dy, z ) board) range
  in
    left && right

isRidden : Coords -> Dict Coords Pai -> Bool
isRidden ( x, y, z ) board =
  List.range -1 1
    |> List.concatMap (\dy ->
        List.range -1 1
          |> List.map (\dx -> ( x + dx, y + dy, z + 1)))
    |> List.any (\coords -> Dict.member coords board)

isMatch : Pai -> Pai -> Bool
isMatch (Pai char1 _) (Pai char2 _) =
  (List.member char1 huapaiChars && List.member char2 huapaiChars) ||
  (List.member char1 sijipaiChars && List.member char2 sijipaiChars) ||
  (char1 == char2)



-- VIEW --

view : Model -> Html Msg
view model =
  div [ id "elm-area" ]
    [ div [ class "svg-wrapper" ]
        [  Svg.svg
            [ SAttr.width "760"
            , SAttr.height "550"
            , SAttr.viewBox <| "0 0 760 550"
            ]
            [ boardView model ]
        ]
    , button [ onClick Undo ] [ text "undo" ]
    ]

boardView : Model -> Svg Msg
boardView { board, hold } =
  let
    pais =
      board
        |> Dict.toList
        |> List.sortBy (\( ( _, _, z ), _ ) -> z)
        |> List.map tileView

    selected =
      [()]
        |> List.filterMap (\_ -> hold)
        |> List.map holdView
  in
    Svg.g []
      [ Svg.g [ SAttr.class "board" ] pais
      , Svg.g [ SAttr.class "selected" ] selected
      ]

tileView : PaiOnBoard -> Svg Msg
tileView (( coords, (Pai char colorOverlay) ) as pob) =
  let
    base =
      [ Svg.rect
          [ SAttr.x "4"
          , SAttr.y "8"
          , SAttr.rx "5"
          , SAttr.ry "5"
          , SAttr.width "48"
          , SAttr.height "63"
          , SAttr.fill "#E5CA80"
          ] []
      , Svg.rect
          [ SAttr.x "2"
          , SAttr.y "5"
          , SAttr.rx "5"
          , SAttr.ry "5"
          , SAttr.width "48"
          , SAttr.height "63"
          , SAttr.fill white
          ] []
      , Svg.rect
          [ SAttr.x "0"
          , SAttr.y "0"
          , SAttr.rx "5"
          , SAttr.ry "5"
          , SAttr.width "48"
          , SAttr.height "63"
          , SAttr.fill white
          , onClick <| PaiClicked pob
          ] []
      , Svg.text_
          [ SAttr.fontSize "90"
          , SAttr.x "-3"
          , SAttr.y "62"
          , SAttr.fill black
          , SAttr.pointerEvents "none"
          ]
          [ Svg.text <| String.fromChar <| char ]
      ]
    edge =
      [ Svg.rect
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
  in
    Svg.g [ translate coords , SAttr.class "tile" ]
      <| base ++ colorOverlay ++ edge

holdView : PaiOnBoard -> Svg Msg
holdView (( coords, _ ) as pob) =
  Svg.g
    [ translate coords
    , onClick <| PaiClicked pob
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

translate : Coords -> Svg.Attribute msg
translate ( x, y, z ) =
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
    , fengpaiChars |> List.concatMap (List.repeat 4)
    , [ baiChar, faChar, zhongChar ]
        |> List.concatMap (List.repeat 4)
    , huapaiChars
    , sijipaiChars
    ]
    |> List.map (\char -> Pai char (colorPai char))

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

fengpaiChars : List Char
fengpaiChars =
  List.range 0 3
    |> List.map (\i -> i + 0x1F000) -- '\u{1F000}' = '🀀'
    |> List.map Char.fromCode

baiChar : Char
baiChar =   '\u{1F006}' --'🀆'

faChar : Char
faChar =    '\u{1F005}' --'🀅'

zhongChar : Char
zhongChar = '\u{1F004}' --'🀄'


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

-- COLOR --

colorPai : Char -> ColorOverlay
colorPai char =
  case char of
    '\u{1F006}' -> colorHole white --'🀆'
    '\u{1F005}' -> colorHole green --'🀅'
    '\u{1F004}' -> colorHole red   --'🀄'
    '\u{1F010}' -> coloryizuo      --'🀐'
    '\u{1F014}' -> colorWuzuo      --'🀔'
    '\u{1F016}' -> colorQizuo      --'🀖'
    '\u{1F018}' -> colorjiuzuo     --'🀘'
    p ->
      if      wanziChars   |> List.member p then
        colorLowerHalf red
      else if fengpaiChars |> List.member p then
        colorHole blue
      else
        []

colorHole : String -> ColorOverlay
colorHole color =
  Svg.rect
      [ SAttr.x "3"
      , SAttr.y "3"
      , SAttr.width "43"
      , SAttr.height "57"
      , SAttr.fill color
      , SAttr.class "blend-lighten"
      , SAttr.pointerEvents "none"
            ]
      []
    |> List.singleton

colorLowerHalf : String -> ColorOverlay
colorLowerHalf color =
  Svg.rect
      [ SAttr.x "0"
      , SAttr.y "28"
      , SAttr.width "47"
      , SAttr.height "30"
      , SAttr.fill color
      , SAttr.class "blend-lighten"
      , SAttr.pointerEvents "none"
      ]
      []
    |> List.singleton

coloryizuo =
  [ Svg.rect
      [ SAttr.x "16"
      , SAttr.y "7"
      , SAttr.width "20"
      , SAttr.height "6"
      , SAttr.fill red
      , SAttr.class "blend-lighten"
      , SAttr.pointerEvents "none"
      ]
      []
  , Svg.rect
      [ SAttr.x "14"
      , SAttr.y "15"
      , SAttr.width "7"
      , SAttr.height "3"
      , SAttr.fill yellow
      , SAttr.class "blend-lighten"
      , SAttr.pointerEvents "none"
      ]
      []
  ]

colorWuzuo =
  Svg.rect
      [ SAttr.x "19"
      , SAttr.y "18"
      , SAttr.width "12"
      , SAttr.height "30"
      , SAttr.fill red
      , SAttr.class "blend-lighten"
      , SAttr.pointerEvents "none"
      ]
      []
    |> List.singleton

colorQizuo =
  Svg.rect
      [ SAttr.x "19"
      , SAttr.y "0"
      , SAttr.width "12"
      , SAttr.height "22"
      , SAttr.fill red
      , SAttr.class "blend-lighten"
      , SAttr.pointerEvents "none"
      ]
      []
    |> List.singleton

colorjiuzuo =
  Svg.rect
      [ SAttr.x "19"
      , SAttr.y "0"
      , SAttr.width "12"
      , SAttr.height "63"
      , SAttr.fill red
      , SAttr.class "blend-lighten"
      , SAttr.pointerEvents "none"
      ]
      []
    |> List.singleton

white  = "#FDF9EE"
black  = "#333333"
red    = "#AA0C0C"
yellow = "#D5AA00"
green  = "#15870C"
blue   = "#0C3D97"



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
