module Common(Node,Edge)where
type alias Node  = {  coord : (Int,Int)
                   ,  pre  : Bool
                   ,  pCoor : (Int,Int)
                   ,  name : String
                   ,  start: Bool
                   ,  fin  : Bool
                   }
type alias Edge = { route:(String,String)
                  , token:List String
                  }
