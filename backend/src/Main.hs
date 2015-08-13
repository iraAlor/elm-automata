{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeFamilies, QuasiQuotes, TemplateHaskell, ViewPatterns, OverloadedStrings #-}
module Main where
import Data.ByteString.Internal (c2w)
import           Prelude         
import Yesod.Core
import qualified Data.ByteString.Lazy as Why
import GHC.Generics
import Data.Default
import Yesod 
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import Data.Map as Map
import Yesod.Default.Util
import Data.List as List
import Data.Text as Text
import Data.Text.Encoding as Text
import Data.Aeson 
import Data.Aeson.TH 
import Data.Aeson.Types
import Re as Re

data App = App

data Node = Node { name:: Text
                 , start :: Bool
                 , final :: Bool
                 , coord :: (Int,Int)
                 }

data Edge = Edge {route::(Text,Text)
                 ,token::[Text]
                 }

data Graph = Graph { nodes :: [Node]
                   , edges :: [Edge]
                   }

$(deriveJSON defaultOptions ''Node)
$(deriveJSON defaultOptions ''Edge)
$(deriveJSON defaultOptions ''Graph)

instance Yesod App 

mkYesod "App" [parseRoutes|
/         HomeR     GET
/download/#Text DownloadR GET
/testRe/#Text/#Text TestReR GET
/testStr/#Text/#Text TestStrR GET
/elm.js ElmR    GET
|]


getHomeR = do
  s <-liftIO $ readFile "temp.html"
  let ts = Text.pack s
  return $ TypedContent  typeHtml $ toContent ts
getElmR = do
  s <-liftIO$ readFile "elm.js"
  let ts = Text.pack s
  return $ TypedContent typeJavascript $ toContent ts


transEdge x = let ((a,b),tkn) = (route x,token x)
              in
              List.map (\j->((a,j),b)) tkn

convertMac gr  =let (nd,ed) = (nodes gr,edges gr)
                    ndls  = Set.fromList $ List.map (\x -> name x) nd
                    edls  = Map.fromList $ List.concat $ List.map transEdge ed
                    fin = Set.fromList $ List.map (\x-> name x) $ List.filter (\x->final x) nd
                    st  = List.map (\x-> name x) $ List.filter (\x->start x) nd
                    in 
                    if 1 /= List.length st then Nothing else Just (ndls,edls,fin,List.head st)
              
step (ed,st) str 
    |0 == Text.length str = st
    | otherwise = let (hd,tl) = (Text.head str,Text.tail str)
                  in
                  step (ed,ed ! (st,(Text.singleton hd))) tl

executeAut (st,ed,fin,sta) str = let last = step (ed,sta) str 
                                 in
                                 if  Set.member last fin then Text.pack "1" else Text.pack "2"

getTestStrR jMa str = do
    let graph = Data.Aeson.decode( Why.pack$ List.map c2w $ Text.unpack jMa)
    case graph of
        Nothing -> return $ TypedContent typePlain $ toContent $ Text.pack "0"
        Just x  -> let gr = convertMac x
                   in 
                   case gr of 
                   Nothing -> return $ TypedContent typePlain $ toContent $ Text.pack "3"
                   Just y -> return $ TypedContent typePlain $ toContent $ executeAut y str
    

getTestReR re dmac = do
  let graph = Data.Aeson.decode ( Why.pack $ List.map c2w $ Text.unpack dmac)
  case graph of
       Nothing -> return $ TypedContent typePlain $ toContent $ Text.pack "-1"
       Just x -> let mac = convertMac x
                 in
                 case mac of
                 Nothing -> return $ TypedContent typePlain $ toContent$ Text.pack "-2"
                 Just y -> return $ TypedContent typePlain $ toContent $ Re.processRe (Text.unpack re) y

getDownloadR a = do
    addHeader "Content-Disposition" "attachment; filename=\"auto\"" 
    return $ TypedContent typeJson $ toContent $ a

main = warp 3000 App
