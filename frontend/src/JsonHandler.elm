module JsonHandler(encodeGraph,decodeGraph) where
import Json.Encode as E
import Json.Decode as D
import Json.Decode exposing (..)
import Common exposing (Node,Edge)
import List exposing (map)

--Encode Section
encodeNode n = E.object
               [ ("name", E.string n.name)
               , ("start",E.bool n.start)
               , ("final",E.bool n.fin)
               , ("coor", E.list [E.int (fst n.coord), E.int (snd n.coord)])
               ]



conTL (a,b)= E.list [E.string a, E.string b] 
convTok ls = E.list (List.map (\x-> E.string x) ls)
encodeEdge e = E.object
             [ ("route",  conTL e.route )
             , ("token",  convTok e.token)
             ]

encodeGraph (nodes,edges) =
  let nl =  List.map encodeNode nodes |> E.list
      ne =  List.map encodeEdge edges |> E.list
      gr = E.object [ ("nodes",nl),("edges",ne)]
  in E.encode 0 gr 








--Decode Section
nodeTemp = { coord = (0,0)
           , pre = False
           , pCoor = (0,0)
           , name  = ""
           , start = False
           , fin = False
           }

edgeTemp = { route = ("","")
           , token = []
           }

decodeNode= D.object4(,,,)  
                      ("name"  := D.string) 
                      ("start" := D.bool) 
                      ("final" := D.bool)
                      ("coor"  := (D.tuple2 (,) D.int D.int))


decodeEdge= D.object2 (,)  ("route" :=(D.tuple2 (,) D.string D.string))
                           ("token" :=(D.list D.string))


decodeGraph str = let h1 = D.object2 (,) ("nodes" := D.list decodeNode) 
                      h2 = h1 ("edges" := D.list decodeEdge)
                      ndG=List.map(\(a,b,c,d) ->{nodeTemp|name<-a,start<-b,fin<-c,coord<-d})
                      edG=List.map(\(a,b)->{edgeTemp|route<-a,token<-b})
                      (nd,ed,fl) = let res  = D.decodeString h2 str 
                                in case res of
                                  Ok val -> (fst val,snd val,True)
                                  _      -> ([],[],False)
                  in
                     (ndG nd,edG ed,fl)


