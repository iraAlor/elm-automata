module Renderer where
import Signal exposing(..)
import Graphics.Element exposing (..)
import Graphics.Collage exposing(..)
import World exposing(runner,Node)
import UI exposing(aNField,buttons,dEField,aEField,dNField,background)
import Mouse exposing(..)
import Color exposing(..)
import List
import Window exposing(dimensions)
scene (w,h) locs =
    let drawCircle node = let (x,y) = node.coord in
    circle 20
    |> filled (hsla (toFloat x) 0.9 0.6 0.7)
    |> move (toFloat x - toFloat w / 2, toFloat h / 2 - toFloat y)
    |> rotate (toFloat x)
    in
    layers
    [ collage w h (List.map drawCircle locs)
    ]


render (state,ed) a b c d (w,h) =
    let nodes = scene (w,h) state in
    let cen   = flow outward ([(background w h)]++[nodes]) in 
    let cont  = flow down (List.map2 (\el1 el2-> flow right ([el1]++[el2])) buttons [a,b,c,d]) in
    flow  down ([cen]++[cont])


main = render<~runner~aNField~aEField~dEField~dNField~dimensions
