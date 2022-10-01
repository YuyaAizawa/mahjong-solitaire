module Main exposing (main)

import Array exposing (Array)
import Browser
import Dict exposing (Dict)
import Html exposing (Html, div, text, button)
import Html.Attributes as HAttr exposing (id, class)
import Html.Events exposing (onClick)
import Svg exposing (Svg)
import Svg.Attributes as SAttr



main =
  Browser.sandbox
    { init = init
    , view = view
    , update = update
    }



-- MODEL --

type alias Model =
  { board : Dict Coords Pai
  , hold : Maybe Coords
  }

type alias Coords = ( Int, Int )

type Pai = Pai Char

init : Model
init =
  { board = initBoard
  , hold = Nothing
  }

initBoard =
  List.range 1 4
    |> List.concatMap (\y ->
        List.range 1 9
          |> List.map (\x -> ( ( x, y ), wanzi x )))
    |> Dict.fromList



-- UPDATE --

type Msg
  = PaiClicked Int Int

update : Msg -> Model -> Model
update msg { board, hold } =
  case msg of
    PaiClicked x y ->
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
