module Renderer where
import Signal exposing(..)
import Graphics.Element exposing (..)
import Graphics.Collage exposing(..)
import World exposing(runner,Node,rad)
import UI exposing(..)
import Mouse exposing(..)
import Color exposing(..)
import List
import Window exposing(dimensions)
import EdgeHelper exposing(genEdge)
import Text exposing(fromString)
scene (w,h) locs edges =
    let drawText node =  let (x,y) = node.coord in 
    fromString (node.name)   
    |>text
    |>move (toFloat x - toFloat w/2,toFloat h/2 - toFloat y)
    in
    let drawCircle node = let (x,y) = node.coord in
    circle rad
    |> filled (blue)
    |> move (toFloat x - toFloat w / 2, toFloat h / 2 - toFloat y)
   -- |> rotate (toFloat x)
    in
    let finS = List.filter (\el ->el.fin) locs in
    let drawFin node = let (x,y) = node.coord in
    circle (rad*0.5)  
    |> filled (lightBlue)
    |> move (toFloat x - toFloat w/2,toFloat h/2 - toFloat y)
    in
    let star = List.filter (\el->el.start) locs in
    let drawTri node =let (x,y) = node.coord in
    ngon 3 (rad/3) 
    |> filled orange
    |> rotate (degrees 0)
    |> move(toFloat x - rad - toFloat w/2,toFloat h/2 - toFloat y)
    in
    let drawEdge =genEdge locs (w,h)
    in
    layers
    ((List.map drawEdge edges)++[collage w h (List.map drawCircle locs)]++
      [collage w h (List.map drawFin finS)]++[collage w h (List.map drawTri star)]
      ++[collage w h (List.map drawText locs)])

render (state,ed) a b c d e f g i j(w,h) =
    let nodes = scene (w,h) state  ed in
    let helper l ls = flow down (List.map2 (\el1 el2-> flow right ([el1]++[el2])) l ls ) in
    let cen   = flow outward ([(background w h)]++[nodes]) in 
    let g1    = helper buttons [a,b,c,d] in
    let g2    = helper buttons2 [e,f,g] in
    let g3    = helper buttons3 [i,j] in
    let g4    = buttons4 in
    let cont  = flow right ([g1]++[g2]++[g3]++g4) in
    flow  down ([cen]++[cont])


main = render<~runner~aNField~aEField~dEField~dNField~startFd~finalAd~finalRm~testStrF~testReF~dimensions
