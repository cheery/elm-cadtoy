module Circle exposing (..)

import Playground exposing (..)

type alias Point = (Number, Number)

type Selection
    = Blank
    | First Point
    | Second Point Point
    | Done Point Point Point

main =
  game view update Blank

view computer state =
  [ case state of
      Blank -> group []
      First (x0,y0) -> group [
        move x0 y0 (circle black 2) ]
      Second (x0,y0) (x1,y1) -> group [
        move x0 y0 (circle black 2),
        move x1 y1 (circle black 2),
        finished_circle (x0,y0) (x1,y1)
            (computer.mouse.x, computer.mouse.y)
            (rgb 230 230 230)
            computer
        ]
      Done (x0,y0) (x1,y1) (x2,y2) -> group [
        move x0 y0 (circle black 2),
        move x1 y1 (circle black 2),
        move x2 y2 (circle black 2),
        finished_circle (x0,y0) (x1,y1) (x2,y2)
            (rgb 200 250 200)
            computer
          ]
  , group [
        circle lightPurple 3,
        seltext state
    ]
      |> move computer.mouse.x computer.mouse.y
      |> fade (if computer.mouse.down then 0.2 else 1)
  ]

finished_circle a b c color computer =
    case (find_center a b c) of
        Nothing ->
            words red "Unstable circle"
             |> move computer.mouse.x computer.mouse.y
             |> move 75 -12
        Just (x3,y3) ->
            move x3 y3 (circle color
                (distance a (x3,y3)))

seltext state = case state of
  Blank -> words black "Select 1st point"
      |> moveX 60
      |> moveY 2
  First _ -> words black "Select 2nd point"
      |> moveX 60
      |> moveY 2
  Second _ _ -> words black "Select 3rd point"
      |> moveX 60
      |> moveY 2
  Done _ _ _ -> words black "Circle!"
      |> moveX 60
      |> moveY 2

update : Computer -> Selection -> Selection
update computer state =
  if computer.mouse.click then
      case state of
        Blank ->
            First (computer.mouse.x, computer.mouse.y)
        First a ->
            Second a (computer.mouse.x, computer.mouse.y)
        Second a b ->
            Done a b (computer.mouse.x, computer.mouse.y)
        Done a b c ->
            Blank
  else
      state

find_center : Point -> Point -> Point -> Maybe Point
find_center i j k =
  let
    q = midplane i j
    w = midplane j k
  in plane_intersect q w


midplane : Point -> Point -> (Point,Number)
midplane a b =
    let d = delta a b
        q = scaled d (1/magnitude d)
    in (q, dot q (midpoint a b))

midpoint : Point -> Point -> Point
midpoint (x1,y1) (x2,y2) = ((x1+x2)/2, (y1+y2)/2)

distance a b = magnitude (delta a b)
    
magnitude : Point -> Number
magnitude point = sqrt (dot point point)

dot : Point -> Point -> Number
dot (x1,y1) (x2,y2) = x1*x2 + y1*y2

delta : Point -> Point -> Point
delta (x1,y1) (x2,y2) = (x1-x2, y1-y2)

plus : Point -> Point -> Point
plus (x1,y1) (x2,y2) = (x1+x2, y1+y2)

scaled : Point -> Number -> Point
scaled (x,y) s = (x*s, y*s)

plane_intersect : (Point,Number) -> (Point,Number) -> Maybe Point
plane_intersect ((a,b),d1) ((c,d),d2) =
    let det = a*d - b*c in
        if det == 0 then
            Nothing
          else
            Just ((d1*d - d2*b)/det,
                  (d2*a - d1*c)/det)
