module Re(processRe) where
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Data.Set (Set)
import Data.Text as Text
import qualified Data.Set as Set
import Data.Map (Map)
{- Target CFG
 - S->J
 - J->K'/'J | KJ | K
 - K->T |T*
 - T->Sigma | (S)
 -}

data Node =  
       Op2 O2 Node Node
      |Leaf Char
      |Op1 O1 Node
    deriving Show
data O2 =
   Concat
 | Union
data O1 = Kleene

 deriving Show



sigma = noneOf "*|()"
fin = do 
      a<-sigma
      return $ Leaf a

term = try (parenP start) <|>  try fin

klee = do 
       res<- term
       char '*'
       return $ Op1 Kleene res

   

parenP :: Parser a -> Parser a
parenP p = do 
             char '('
             x<-p
             char ')'
             return x

sing = try term <|> try klee 
       
uni = do
      a<-sing
      char '|'
      b<-dcomp
      return $ Op2 Union a b

conc = do
      a<-sing
      b<-dcomp 
      return $ Op2 Concat a b

dcomp = try uni <|> try conc <|> try sing
         


start = dcomp


genSingle c counter = (sta,ed,fi,start,nc)
    where
     id0  = genTxt counter
     (id1,nc)  = (genTxt (counter+1) counter+2)
     sta  = Set.fromList [id0,id1]
     fi   =Set.singleton id1
     ed   = Map.singleton (id0,Text.singleton c) id1
     start = id1

collapse s1 s2 fin = Map.union p1 p2
         where 
         p1 = focus s1 fin
         p2 = focus s2 fin


genUnion  a b cntr = let (sta1,ed1,fi1,start1,nc) = conv a cntr
                         (sta2,ed2,fi2,start2,nnc) = conv b nc
                         sId = genTxt nnc
                         (fId,fc) = (genTxt (nnc+1),nnc+2)
                         fi = Set.singleton fId
                         sta = Set.union sta2 $ Set.union sta1 $Set.union fi (Set.singleton sId)
                         start = sId
                         ed = Map.union ed2 $ Map.union ed1 $ collapse fi1 fi2 fId
                         in
                         (sta,ed,fi,start,fc)

focus s1 fin = Set.fold (\x acc -> Map.union acc $ Map.singleton (x,Text.empty) fin) Map.empty s1

genConcat a b cntr =let (sta1,ed1,fi1,start1,nc) = conv a cntr
                        (sta2,ed2,fi2,start2,nnc) = conv a nc
                        ed = Map.union ed1 $ Map.union ed2 $ focus fi1 start2
                        sta = Set.union sta1 sta2
                        in
                        (sta,ed,fi2,start1,ncc)
                        

genKleen a cntr = let (sta1,ed1,fi1,start1,nc) = conv a cntr
                      (sId,fId,ncc) = (genTxt nc,genTxt (nc+1),(nc+2))
                      edp = Map.union $ Map.singleton (sId,Text.empty) start1 $ Map.singleton (sId,Text.empty) fId
                      f   = Set.findMin fi1
                      edp1 = Map.union edp $ Map.singleton (f,Text.empty) start1
                      edp2 = Map.union edp1 $ Map.singleton (f,Text.empty) fId
                      edp3 = Map.union edp2 $ ed1
                      sta = Set.union sta1 $ Set.fromList [sId,fId]
                      fi  = Set.singleton fId
                      in
                      (sta,edp3,fi,sId,ncc)



genTxt a = Text.pack $ show a
conv node counter = case node of 
   Op2 Concat a b -> genConcat a b counter
   Op2 Union  a b -> genUnion a b counter
   Leaf c -> genSingle c counter 
   Op1 Kleene a-> genKleen a counter


processRe input = case parse start "RE conv"(filter (\x->x/=' ') input) of
  Left err -> "shti "++ show err
  Right val -> show val


