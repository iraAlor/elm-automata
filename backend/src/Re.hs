module Re(processRe) where
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Data.Set (Set)
import Data.Text as Text
import qualified Data.Set as Set
import Data.Map (Map)
import Data.Map as Map
import Data.List as List
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
 deriving Show
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

genSingle :: (Show a,Num a)=>Char-> a -> (Set Text,Map (Text,Text) (Set Text),Set Text,Text,a)
genSingle c counter = (sta,ed,fi,start,nc)
    where
     id0  = genTxt counter
     (id1,nc)  = (genTxt (counter+1), counter+2)
     sta  = Set.fromList [id0,id1]
     fi   =Set.singleton id1
     ed   = Map.singleton (id0,Text.singleton c) (Set.singleton id1)
     start = id1

collapse s1 s2 fin = myMapUnion p1 p2
         where 
         p1 = focus s1 fin
         p2 = focus s2 fin

myMapUnion = Map.unionWith (\a b -> Set.union a b)

genUnion :: (Show a,Num a)=>Node->Node-> a -> (Set Text,Map (Text,Text) (Set Text),Set Text,Text,a)
genUnion  a b cntr = let (sta1,ed1,fi1,start1,nc) = conv a cntr
                         (sta2,ed2,fi2,start2,nnc) = conv b nc
                         sId = genTxt nnc
                         (fId,fc) = (genTxt (nnc+1),nnc+2)
                         fi = Set.singleton fId
                         sta = Set.union sta2 $ Set.union sta1 $Set.union fi (Set.singleton sId)
                         start = sId
                         edp = myMapUnion ed2 $ myMapUnion ed1 $ collapse fi1 fi2 fId
                         ed = myMapUnion edp $ Map.singleton (sId,Text.empty) $  Set.fromList [start1,start2]
                         in
                         (sta,ed,fi,start,fc)

focus s1 fin =let help x acc = myMapUnion acc $ Map.singleton (x,Text.empty) $ Set.singleton fin
              in
              Set.fold help Map.empty s1

genConcat :: (Show a,Num a)=>Node->Node-> a -> (Set Text,Map (Text,Text) (Set Text),Set Text,Text,a)
genConcat a b cntr =let (sta1,ed1,fi1,start1,nc) = conv a cntr
                        (sta2,ed2,fi2,start2,nnc) = conv a nc
                        ed = myMapUnion ed1 $ myMapUnion ed2 $ focus fi1 start2
                        sta = Set.union sta1 sta2
                        in
                        (sta,ed,fi2,start1,nnc)
                        
genKleen::(Show a,Num a)=> Node-> a ->(Set Text,Map (Text,Text)(Set Text),Set Text,Text,a)
genKleen a cntr = let (sta1,ed1,fi1,start1,nc) = conv a cntr
                      (sId,fId,ncc) = (genTxt nc,genTxt (nc+1),(nc+2))
                      edp = Map.singleton (sId,Text.empty) $ Set.fromList [start1,fId]
                      f   = Set.findMin fi1
                      edp1 = myMapUnion edp $ Map.singleton (f,Text.empty)$  Set.fromList [start1,fId]
                      edp2 = myMapUnion edp1 $ ed1
                      sta = Set.union sta1 $ Set.fromList [sId,fId]
                      fi  = Set.singleton fId
                      in
                      (sta,edp2,fi,sId,ncc)


genTxt :: (Show a)=> a -> Text
genTxt a = Text.pack $ show a

conv node counter = case node of 
   Op2 Concat a b -> genConcat a b counter
   Op2 Union  a b -> genUnion a b counter
   Leaf c -> genSingle c counter 
   Op1 Kleene a-> genKleen a counter


sig (ed)  = Set.toList $ Map.foldWithKey (\(a,b) c acc -> Set.insert b acc) Set.empty ed

eClosure::(Ord t)=>Set t -> Map (t,Text) (Set t) -> Set t -> Set t
eClosure currState edges prev = let help x = Map.findWithDefault Set.empty (x,Text.empty) edges
                                    h1 el (r,p) = if Set.notMember el p then (Set.union r $ help el,Set.insert el p) else (r,p)
                                    (nxt,np) = Set.fold h1 (Set.empty,prev) currState
                                    total= eClosure nxt edges np 
                                    in 
                                    if not (Set.isSubsetOf currState prev) then total else prev



convertAutoToDFA::(Ord k)=>Mach k->DFA (Set k)
convertAutoToDFA (states,edges,final,start) = (nstates,delta,sigma,fin,anustart)
 where 
   nstates = Set.fromList $ List.map (\x->Set.fromList x) $ List.subsequences $ Set.toAscList states
   help x a = Map.findWithDefault Set.empty (x,a) edges
   sigma = sig edges
   delta q a = let sq = Set.toAscList q 
                   p0 = List.foldr(\x a->Set.union x a) Set.empty $ [help e a |e<-sq]
                   in  eClosure p0 edges Set.empty
   anustart = eClosure (Set.singleton start) edges Set.empty 
   fin a  = (Set.intersection a final) /=Set.empty

emptyDfa sigmap = (state,delta,sigma,fin,start)
 where
   start = Text.pack "E"
   fin   = Set.empty
   delta q a = q
   state = Set.singleton start
complementDFA::(Ord k)=>DFA k ->DFA k
complementDFA (state,delta,sigma,fin,start) = (state,delta,sigma,nfin,start)
 where
  nfin a = not $ fin a

unionDFA::(Ord t,Ord k)=>DFA t -> DFA k -> DFA (t,k)
unionDFA (s1,d1,sig1,f1,start1) (s2,d2,sig2,f2,start2) = (states,delta,sigma,fin,anustart)
 where
  states =let (sl1,sl2) = (Set.toAscList s1,Set.toAscList s2) in Set.fromList $ [(a,b)|a<-sl1,b<-sl2]
  delta (a,b) c = (d1 a c,d2 b c)
  sigma= sig1
  anustart = (start1,start2)
  fin (a,b) = (f1 a) || (f2 b)


subtractDFA::(Ord t,Ord k)=>DFA t -> DFA k -> DFA (t,k)
subtractDFA (s1,d1,sig1,f1,start1) (s2,d2,sig2,f2,start2) = (states,delta,sigma,fin,anustart)
 where
  states =let (sl1,sl2) = (Set.toAscList s1,Set.toAscList s2) in Set.fromList $ [(a,b)|a<-sl1,b<-sl2]
  delta (a,b) c = (d1 a c,d2 b c)
  sigma= sig1
  anustart = (start1,start2)
  fin (a,b) = (f1 a) && (not $ f2 b)

checkEmp::(Ord a)=> DFA a -> Bool
checkEmp (states,delta,sigma,fin,start) =let helper curr pr = let next = [ delta q c |q<-curr,c<-sigma]
                                                                  prm = Set.union (Set.fromList next) pr
                                                                  visted  = helper next prm
                                                                  in if []==curr then pr else visted
                                             reachable = helper [start] Set.empty
                                             in 
                                             Set.fold (\x acc ->acc ||(fin x)) False reachable

type Mach a = (Set a,Map (a,Text) (Set a),Set a,a)
type DFA st = (Set st,st->Text->st,[Text],st->Bool,st)

checkEQ::(Ord a,Ord b)=> DFA a-> DFA b -> Bool
checkEQ m1 m2 = checkEmp p3
 where
    p1 = subtractDFA m1 m2
    p2 = subtractDFA m2 m1
    p3 = unionDFA p1 p2

convertToDFA (st,edge,fi,start) = (st,delta,sigma,fin,start)
 where sigma = sig edge
       delta a c = edge ! (a,c)
       fin a = Set.member a fi

processRe input desc = case parse start "RE conv"(List.filter (\x->x/=' ') input) of
  Left err -> Text.pack "0"
  Right val -> let (a,b,c,d,e) = conv val 0 
                   dfa0 = convertAutoToDFA (a,b,c,d) 
                   dfa1 = convertToDFA desc 
                   res  = checkEQ dfa0 dfa1 
                   in if res then Text.pack "1" else Text.pack "2"




