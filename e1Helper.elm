module EdgeHelper(genEdge) where
import Graphics.Collage exposing(..)
import Text exposing(fromString)
import World exposing(Node,Edge)
import List
import String exposing(join)
import Color exposing (red)

def ={ coord = (10,10)
     , pre   = False
     , pCoor = (0,0)
     , name  = ""
     }

belzGen start end cont = \t->  ((1-t)^2*start+2*(1-t)*t*cont+t*t*end)
getPath (xs,ys) (xe,ye) a b   =  let (x,y) = (toFloat (xe-xs),toFloat (ye-ys)) in
                                 let (r,t) = toPolar(x,y) in
                                 let (mdx,mdy) = (((r/2*(cos t)+toFloat xs)),( (r/2*(sin t)+toFloat ys)))in
                                 ([((toFloat xs) - a,b- (toFloat ys)),((toFloat xe)-a,b-(toFloat ye))],mdx+5,mdy+5,t)



genEdge nodes (w,h) edge= let (start,end) = edge.route in
                          let helps n (a,b)= (if n.name == start then n else a,if n.name == end then n else b) in
                          let (sNode,eNode) = List.foldl helps (def,def) nodes in
                          let (j,k)        = (sNode.coord,eNode.coord) in
                          let (points,mdx,mdy,th) = getPath j k (toFloat w/2) (toFloat h/2) in
                          let txt  =  Graphics.Collage.text (fromString (toString (j,k)))
                                      |>rotate -th
                                      |>move( mdx-toFloat w/2 ,toFloat h/2-mdy) in
                          let pt = path points|>traced {defaultLine|width<-10} in
                          let tri = ngon 3 10|>filled red|>move(mdx-toFloat w/2,  mdy-toFloat h/2) in
                          collage w h [pt,txt]

                       
