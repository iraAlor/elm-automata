module Connect where

import Common exposing (Node,Edge)
import Signal as S
import World exposing (runner)
import UI exposing (saveButt)
import Task exposing (..)
import Http exposing(defaultSettings)
import Json.Encode exposing(..)
import List 
saveSig = S.sampleOn saveButt.signal runner

encodeNode n = object
               [ ("name", string n.name)
               , ("start", bool n.start)
               , ("final", bool n.fin)
               ]
conTL (a,b)= list [string a, string b] 
convTok ls = list (List.map (\x-> string x) ls)
encodeEdge e = object
             [ ("route",  conTL e.route )
             , ("token",  convTok e.token)
             ]

encodeGraph (nodes,edges) =
  let nl =  List.map encodeNode nodes |> list
      ne =  List.map encodeEdge edges |> list
      gr = object [ ("nodes",nl),("edges",ne)]
  in encode 0 gr 
genFile (nls,els) = 
  let jgr = encodeGraph(nls,els) 
      toUrl ="http://localhost:3000/download"
  in
      Http.send defaultSettings 
      { verb = "POST"
      , headers = []
      , url = toUrl
      , body =Http.string jgr
    }
port requests : Signal (Task Http.RawError Http.Response)
port requests =
  S.map genFile (saveSig)
