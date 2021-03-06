module Renderer where
import Signal exposing(..)
import Graphics.Element exposing (..)
import Graphics.Collage exposing(..)
import World exposing(updateC,rad)
import Common exposing (Node,Edge)
import UI exposing(..)
import Mouse exposing(..)
import Color exposing(..)
import List
import Window exposing(dimensions)
import EdgeHelper exposing(genEdge)
import Text exposing(fromString)
import JsonHandler exposing(..)

import Task exposing (..)


--Ports and Corresponding Signals



saveSig = Signal.sampleOn saveButt.signal runner

port conn: Signal String
port conn = Signal.map encodeGraph saveSig

getSig t = let helpy = Signal.map encodeGraph runner 
               h2 = Signal.map2 (\a b ->(a,b)) t
               bs = Signal.sampleOn t
               in
               bs(h2  helpy)


port tStr: Signal (String,String)
port tStr = getSig testStrSig
port tRe: Signal (String,String)
port tRe = getSig testReSig


                                

--Javascript gives elm the text description of the automata
port upld: Signal String
--Elm  tells Javascript if file could be parsed
port resUpJ: Signal Bool
port resUpJ = Signal.map  (\(a,b,c)->c) resUpload

--Parses contents of file into an automata if possible
resUpload = Signal.map (\x->(decodeGraph x)) upld

--Model

mMySig = Signal.merge mySig (Signal.map (\x->NewWorld x)resUpload)
runner = Signal.foldp updateC ([],[]) mMySig



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

render (state,ed) ls1 ls2 ls3 (w,h) =
    let nodes = scene (w,h) state  ed in
    let helper l ls = flow down (List.map2 (\el1 el2-> flow right ([el1]++[el2])) l ls ) in
    let cen   = flow outward ([(background w h)]++[nodes]) in 
    let g1    = helper buttons ls1 in
    let g2    = helper buttons2 ls2 in
    let g3    = helper buttons3 ls3 in
    let g4    = flow down buttons4 in
    let cont  = flow right ([g1]++[g2]++[g3]++[g4]) in
    flow  down ([cen]++[cont])


main = render<~runner~fields~fields2~fields3~dimensions
