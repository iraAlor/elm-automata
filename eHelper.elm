module EdgeHelper(genEdge) where
import Graphics.Collage exposing(..)
import Text exposing(fromString)
import World exposing(Node)
getPath rad (xs,ys) (xe,ye) = let (x,y) = ( (toFloat (xe-x)),(toFloat (ye-y))) in
                              let mid = ((round .5*x)+xs,(round .5*y)+ys) in
                              let (r,th) = toPolar (x,y) in
                              let ratio = rad/r in
                              let rat2 =(rad-r)/r in
                              let start = ((round ratio*x)+xs,(round ratio*y)+ys) in
                              let end =((round rat2*x)+ys,(round rat2*y)+xs) in
                                                              ( mid,start,end,th)
getTok tok = case tok  of
  "" -> fromString ""
  _-> fromString slice 1 -1 show tok

genEdge n1 n2 tok rad (w,h) = let (mid,start,end,th) = getPath rad n1.coord n2.coord in
                              let lin = traced {defaultLine| width <- 5 }path [start,mid,end] in
                              let (mx,my) = mid in
                              let (ex,ey) = end in
                              let txt = move(toFloat mx-toFloat w/2 ,toFloat my - toFloat h/2)  rotate th text getTok tok in
                              let hd  = ngon 3 10|>filled red|>move(toFloat ex - toFloat w/2,toFloat ey -toFloat h/2)|>rotate th in
                              (lin,txt,hd)





                       
