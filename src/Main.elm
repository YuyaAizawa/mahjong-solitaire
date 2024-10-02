module Main exposing (main)

import Array exposing (Array)
import Browser
import Dict exposing (Dict)
import Html exposing (Html, div, text, button, input, label)
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
  , settings : Settings
  }

type alias Coords = ( Int, Int, Int ) -- a pai occupies 2x2x1 Coords

type Pai = Pai Char ColorOverlay
type alias ColorOverlay = List (Svg Msg)

type alias PaiOnBoard = ( Coords, Pai )

type alias Settings =
  { jamAlert : Bool
  , heightColor : Bool
  }

init : () -> ( Model, Cmd Msg )
init _ =
  ( { board = Dict.empty
    , hold = Nothing
    , history = []
    , settings =
      { jamAlert = False
      , heightColor = False
      }
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
  | SettingsUpdated Settings
  | PileUp (Dict Coords Pai)

update : Msg -> Model -> (Model, Cmd Msg)
update msg { board, hold, history, settings } =
  case msg of
    PaiClicked ( clickedCoords, clickedPai ) ->
      let
        model_ =
          if isBlocked clickedCoords board
          then
            Model board hold history settings
          else
            case hold of

              Nothing ->
                Model board (Just ( clickedCoords, clickedPai )) history settings

              Just ( holdCoords, holdPai ) ->
                if clickedCoords == holdCoords then
                  Model board Nothing history settings
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
                    Model board_ Nothing history_ settings
                else
                  Model board hold history settings
      in
        ( model_, Cmd.none )

    Undo ->
      let
        model_ =
          case history of
            [] ->
              Model board Nothing [] settings

            ( ( coords1, pai1 ), ( coords2, pai2 ) ) :: history_ ->
              let
                board_ =
                  board
                    |> Dict.insert coords1 pai1
                    |> Dict.insert coords2 pai2
              in
                Model board_ Nothing history_ settings
      in
        ( model_, Cmd.none )

    SettingsUpdated settings_ ->
      ( Model board hold history settings_
      , Cmd.none
      )

    PileUp board_ ->
      let
        test =
          solve board_
      in
        ( Model board_ Nothing [] settings
        , Cmd.none
        )



-- VIEW --

view : Model -> Html Msg
view model =
  let
    settings =
      model.settings

  in
    div [ id "elm-area" ]
      [ div [ class "svg-wrapper" ]
          (
          [ Svg.svg
              [ SAttr.width "760"
              , SAttr.height "550"
              , SAttr.viewBox <| "0 0 760 550"
              ]
              [ boardView model ]
          , div
              [ class <| "completed" ++ (if Dict.isEmpty model.board then "" else " none") ]
              [ text "クリアー" ]
          , div
              [ class <| "jamed" ++ (if settings.jamAlert && isNohand model.board && not (Dict.isEmpty model.board) then "" else " none") ]
              [ text "詰みです" ]
          ]
          )
      , button [ onClick Undo ] [ text "undo" ]
      , checkbox "jamAlert" "詰みを表示" settings.jamAlert (\b -> { settings | jamAlert = b })
      , checkbox "heightColor" "高さを色で表示" settings.heightColor (\b -> { settings | heightColor = b })
      ]

boardView : Model -> Svg Msg
boardView { board, hold, settings } =
  let
    pais =
      board
        |> Dict.toList
        |> List.sortBy (\( ( _, _, z ), _ ) -> z)
        |> List.map (tileView settings.heightColor)

    selected =
      [()]
        |> List.filterMap (\_ -> hold)
        |> List.map holdView
  in
    Svg.g
        [ SAttr.transform <| "translate(10 10)" ]
        [ Svg.g [ SAttr.class "board" ] pais
        , Svg.g [ SAttr.class "selected" ] selected
        ]

tileView : Bool -> PaiOnBoard -> Svg Msg
tileView heightColor (( coords, (Pai char colorOverlay) ) as pob) =
  let
    face =
      if heightColor then
        case coords of
          ( _, _, 4 ) -> "#FFD8D8"
          ( _, _, 3 ) -> "#F7FFD8"
          ( _, _, 2 ) -> "#D8FFE8"
          ( _, _, 1 ) -> "#D8E8FF"
          _           -> "#F7D8FF"
      else
        white

    char_ =
      if char == baiChar then
        ' '
      else
        char

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
          , SAttr.fill face
          ] []
      , Svg.rect
          [ SAttr.x "0"
          , SAttr.y "0"
          , SAttr.rx "5"
          , SAttr.ry "5"
          , SAttr.width "48"
          , SAttr.height "63"
          , SAttr.fill face
          , onClick <| PaiClicked pob
          ] []
      , Svg.text_
          [ SAttr.fontSize "90"
          , SAttr.x "-3"
          , SAttr.y "62"
          , SAttr.fill black
          , SAttr.pointerEvents "none"
          ]
          [ Svg.text <| String.fromChar <| char_ ]
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

checkbox : String -> String -> Bool -> (Bool -> Settings) -> Html Msg
checkbox name text_ state updater =
  label [ HAttr.name name ]
    [ input
        [ HAttr.type_ "checkbox"
        , HAttr.checked state
        , id name
        , onClick <| SettingsUpdated <| updater <| not state
        ]
        []
    , text text_
    ]



-- SUBSCRIPTION --

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- LOGIC --

type alias AtLeastOne a = ( a, List a )

isMatch : Pai -> Pai -> Bool
isMatch (Pai char1 _) (Pai char2 _) =
  (List.member char1 huapaiChars && List.member char2 huapaiChars) ||
  (List.member char1 sijipaiChars && List.member char2 sijipaiChars) ||
  (char1 == char2)

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

isNohand : Dict Coords Pai -> Bool
isNohand board =
  board
    |> getCandidates
    |> List.isEmpty

getCandidates : Dict Coords Pai -> List ( PaiOnBoard, PaiOnBoard )
getCandidates board =
  let
    sorted =
      board
        |> Dict.toList
        |> List.filter (\( coords, _ ) -> not <| isBlocked coords board)
        |> List.sortBy (\( _, (Pai char _ ) ) -> char)

    grouped =
      sorted
        |> groupByPai
        |> List.map (\( hd, tl ) -> hd :: tl)
  in
    grouped
      |> List.filter (\list -> List.length list > 1)
      |> List.concatMap allPairs

groupByPai : List PaiOnBoard -> List (AtLeastOne PaiOnBoard)
groupByPai list =
  groupByPaiHelper list []

groupByPaiHelper : List PaiOnBoard -> List (AtLeastOne PaiOnBoard) -> List (AtLeastOne PaiOnBoard)
groupByPaiHelper lest acc =
  case lest of
    [] -> acc |> List.reverse

    (( _, pai ) as hd) :: tl ->
      case acc of
        [] -> groupByPaiHelper tl [(hd , [])]

        ( ( _, reprPai ) as repr, group ) :: others ->
          if isMatch pai reprPai then
            groupByPaiHelper tl (( repr, hd :: group ) :: others)
          else
            groupByPaiHelper tl (( hd, [] ) :: ( repr, group ) :: others)

allPairs : List a -> List ( a, a )
allPairs list =
  case list of
    [] ->
      []

    hd :: tl ->
      List.map (Tuple.pair hd) tl ++ allPairs tl



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



-- SEARCH --

solve : Dict Coords Pai -> Bool
solve board =
  let
    ( _, result ) =
      solveHelper 0 (board |> getCandidates) board
        |> Debug.log "solve"
  in
    result

solveHelper : Int -> List ( PaiOnBoard, PaiOnBoard ) -> Dict Coords Pai -> ( Int, Bool )
solveHelper count candidates board =
  if board |> Dict.isEmpty then
    ( count, True )
  else
    case candidates of
      [] ->
        ( count, False )

      ( ( c1, _ ), ( c2, _ ) ) :: restCandidates ->
        let
          nextBoard =
            board
              |> Dict.remove c1
              |> Dict.remove c2

          log =
            case modBy 1000 (count + 1) == 0 of
              True ->
                ((count + 1), nextBoard |> Dict.size ) |> Debug.log "count, restOnBoard"
              False ->
                ( 0, 0 )
        in
          case solveHelper (count + 1) (nextBoard |> getCandidates) nextBoard of
            ( count_, True ) ->
              ( count_, True )

            ( count_, False ) ->
              solveHelper count_ restCandidates board



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
