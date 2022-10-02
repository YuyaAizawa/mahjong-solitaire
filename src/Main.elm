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

type Pai = Pai Char String -- kind, color

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
tileView (( coords, (Pai char color) ) as pob) =
  let
    ( attrX, attrY ) = attrXY coords
  in
    Svg.g
      [ SAttr.class "tile"
      ]
      [ Svg.rect
          [ attrX 4.0
          , attrY 8.0
          , SAttr.rx "5"
          , SAttr.ry "5"
          , SAttr.width "48"
          , SAttr.height "63"
          , SAttr.fill "#E5CA80"
          ] []
      , Svg.rect
          [ attrX 2.0
          , attrY 5.0
          , SAttr.rx "5"
          , SAttr.ry "5"
          , SAttr.width "48"
          , SAttr.height "63"
          , SAttr.fill white
          ] []
      , Svg.rect
          [ attrX 0.0
          , attrY 0.0
          , SAttr.rx "5"
          , SAttr.ry "5"
          , SAttr.width "48"
          , SAttr.height "63"
          , SAttr.fill white
          , onClick <| PaiClicked pob
          ] []
      , Svg.text_
          [ SAttr.fontSize "90"
          , attrX -3.0
          , attrY 62.0
          , SAttr.fill color
          , SAttr.pointerEvents "none"
          ]
          [ Svg.text <| String.fromChar <| char ]
      , Svg.rect
          [ attrX 0.3
          , attrY 0.3
          , SAttr.rx "5"
          , SAttr.ry "5"
          , SAttr.width "47.5"
          , SAttr.height "63.5"
          , SAttr.stroke "#222"
          , SAttr.strokeWidth "1.5"
          , SAttr.fill "none"
          ] []
      ]

holdView : PaiOnBoard -> Svg Msg
holdView (( coords, _ ) as pob) =
  let
    ( attrX, attrY ) = attrXY coords
  in
    Svg.g
      [ onClick <| PaiClicked pob
      ]
      [ Svg.rect
          [ attrX 0.3
          , attrY 0.3
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

attrXY : Coords -> ( Float -> Svg.Attribute msg, Float -> Svg.Attribute msg )
attrXY ( x, y, z ) =
  let
    ox = x * 24 - z * 4 + 15 |> toFloat
    oy = y * 32 - z * 8 + 15 |> toFloat
  in
    ( \x_ -> ox + x_ |> String.fromFloat |> SAttr.x
    , \y_ -> oy + y_ |> String.fromFloat |> SAttr.y
    )



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

colorPai : Char -> String
colorPai char =
  case char of
    '\u{1F006}' -> white --'🀆'
    '\u{1F005}' -> green --'🀅'
    '\u{1F004}' -> red   --'🀄'
    p ->
      if List.member p fengpaiChars then
        blue
      else
        black



-- COLOR --

white = "#FDF9EE"
black = "#333333"
red   = "#970C0C"
green = "#15670C"
blue  = "#0C3D97"



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
