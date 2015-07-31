module EdgeHelper(genEdge) where
import Graphics.Collage exposing(..)
import Text exposing(fromString)
import World exposing(Node,Edge,rad)
import List
import String exposing(join)
import Color exposing (red)

def ={ coord = (10,10)
     , pre   = False
     , pCoor = (0,0)
     , name  = ""
     }

belzGen start end cont = \t->  ((1-t)^2*start+2*(1-t)*t*cont+t*t*end)
getPath (xs,ys) (xe,ye) a b  (d1,d2) mv =  let (x,y) = (toFloat (xe-xs),toFloat (ye-ys)) in
                                           let (r,t) = toPolar(x,y) in
                                           let(mdx,mdy) = (((r/2*(cos t)+toFloat xs)),( (r/2*(sin t)+toFloat ys)))in
                                           let oc =(mdx+(toFloat d1),mdy+(toFloat d2)) in
                                           ([((toFloat (xs+d1)) - a,b- (toFloat (ys+d2))),((toFloat (xe+d1))-a,b-(toFloat (ye+d2)))],mdx+(abs mv),mdy+mv,t,oc) 
sel (a,b) = if a<b then (0,round(0.5*rad),degrees 0,0.125*rad) else (0,round(-0.5*rad),degrees 180,-0.875*rad)


genEdge nodes (w,h) edge= let (start,end) = edge.route in
                          let helps n (a,b)= (if n.name == start then n else a,if n.name == end then n else b) in
                          let (sNode,eNode) = List.foldl helps (def,def) nodes in
                          let (j,k)        = (sNode.coord,eNode.coord) in
                          let (d0,d1,s,mv)  =  sel edge.route in
                          let (points,mdx,mdy,th,(a,b)) = getPath j k (toFloat w/2) (toFloat h/2) (d0,d1) mv in
                          let txt  =  Graphics.Collage.text (fromString (toString (edge.token)))
                                      |>rotate (-th+s)
                                      |>move( mdx-toFloat w/2 ,toFloat h/2-mdy) in
                          let pt = path points|>traced {defaultLine|width<-5} in
                          let tri = ngon 3 10
                                    |>filled red
                                    |>move(a-toFloat w/2,(toFloat h/2)-(b)) 
                                    |>rotate (-th) in
                          collage w h [pt,txt,tri]

                       
