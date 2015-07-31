module Renderer where
import Signal exposing(..)
import Graphics.Element exposing (..)
import Graphics.Collage exposing(..)
import World exposing(runner,Node,rad)
import UI exposing(aNField,buttons,dEField,aEField,dNField,background)
import Mouse exposing(..)
import Color exposing(..)
import List
import Window exposing(dimensions)
import EdgeHelper exposing(genEdge)
import Text exposing(fromString)
scene (w,h) locs edges =
    let drawText node =  let (x,y) = node.coord in 
    fromString node.name    
    |>text
    |>move (toFloat x - toFloat w/2,toFloat h/2 - toFloat y)
    in
    let drawCircle node = let (x,y) = node.coord in
    circle rad
    |> filled (blue)
    |> move (toFloat x - toFloat w / 2, toFloat h / 2 - toFloat y)
   -- |> rotate (toFloat x)
    in
    let drawEdge =genEdge locs (w,h)
    in
    layers
    ((List.map drawEdge edges)++[collage w h (List.map drawCircle locs)]++[collage w h (List.map drawText locs)])

render (state,ed) a b c d (w,h) =
    let nodes = scene (w,h) state  ed in
    let cen   = flow outward ([(background w h)]++[nodes]) in 
    let cont  = flow down (List.map2 (\el1 el2-> flow right ([el1]++[el2])) buttons [a,b,c,d]) in
    flow  down ([cen]++[cont])


main = render<~runner~aNField~aEField~dEField~dNField~dimensions
