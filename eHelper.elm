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
getPath (startX,startY) (endX,endY) (bX,bY) (w,h)  = let pnt = List.map (\a ->(toFloat a) * 0.01) [0..100] in
                                                     let belzX = belzGen startX endX bX in
                                                     let belzY = belzGen startY endY bY in
                                                     (List.map (\t->(belzX t,belzY t)) pnt,belzX (50*0.01),belzY (50*0.01))

thetaP r = asin (100/r)
thetaN r = asin (-100/r)

controlPoint (xs,ys) (xe,ye) theta = let (x,y) = (toFloat (xe-xs),toFloat (ye-ys)) in
                                     let (r,t) = toPolar (x,y) in
                                     let th = theta (r/2) in
                  --                (th, asin (100/r/2),t)
                                     ((round((r/2)*cos th))+xs,(round ((r/2)*sin th))+ys,t)


help (a,b) = if a<b then thetaP else thetaN
mid ls = let ind = (List.length ls)//2 in
         let (a,b) = List.foldl (\x (acc,i) -> (if i==ind then x else acc,i+1)) (-1.1,0) ls in
         a
genEdge nodes (w,h) edge= let (start,end) = edge.route in
                          let helps n (a,b)= (if n.name == start then n else a,if n.name == end then n else b) in
                          let (sNode,eNode) = List.foldl helps (def,def) nodes in
                          let (j,k)        = (sNode.coord, eNode.coord) in
                          let (cx,cy,th)   =  controlPoint  j  k (help (start,end)) in
                          let (points,mdx,mdy) = getPath  j k (toFloat (cx),toFloat (cy)) in
                          let txt  =  Graphics.Collage.text (fromString (toString (cx,cy)))
                                      |>rotate -th
                                      |>move(toFloat cx-toFloat w/2 , toFloat cy - toFloat h/2) in
                          let pt = path points|>traced {defaultLine|width<-10} in
                          let tri = ngon 3 10|>filled red|>move(mdx-toFloat w/2,  mdy-toFloat h/2) in
                          collage w h [pt,txt]

                       
