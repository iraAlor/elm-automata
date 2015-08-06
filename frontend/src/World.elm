module World(runner,rad)  where
import Common exposing(Node,Edge)
import UI exposing(..)
import Signal exposing(..) 
import List exposing(foldl) 
import String 
start = { coord = (10,10)
        , pre   = False
        , pCoor = (0,0)
        , name  = ""
        , start = False
        , fin   = False
        }
sEdge = {route = ("","")
        ,token = []
        }


helpy2 (cx,cy) node = let ((px,py),(mx,my)) = (node.pCoor,node.coord) in
                      let (dx,dy) = ((cx-px),(cy-py)) in
                      {node|coord<-(mx+dx,my+dy),pre<-True,pCoor<-(cx,cy)}

rad = 40.0
contains (cx,cy) (px,py) = (cx-px)^2 +(cy-py)^2 <= (round (rad)^2)


helper (cx,cy) el (acc,ls)  = 
    case acc of
    Nothing->if contains (cx,cy) el.coord then (Just el,ls) else (acc,el::ls)
    _      ->(acc,el::ls)



find co ls = let (b,c) = foldl (helper co) (Nothing,[]) ls in
      case b of 
        Just x -> if x.pre then (Just (helpy2 co x),c) else  (Just {x|pre<-True,pCoor<-co},c)
        _      ->(Nothing,c)

helpy ls co = let (node,nls) = (find co ls) in
   case node of
        Nothing -> setToF nls
        Just n  -> n::nls

setToF ls = List.map (\a->{a|pre<-False}) ls

conHelp el ((a,b),ls,counter) = case (el =='(' || (el ==')'),el,counter<2) of
  (True,_,_) -> ((a,b),ls,counter)
  (_,',',True)  -> ((a,b),ls,counter+1)
  (_,_,True) -> if counter == 0 then ((String.cons el "",b),ls,counter) else ((a,String.cons el ""),ls,counter)
  (_,_,_)    -> ((a,b),String.cons el ls,counter)
  
convertToE str  = let (r,tk,_) = String.foldl conHelp (("",""),"",0) str in
                  let ttk = String.split ","( String.reverse tk) in
                  (r,ttk)
conEdge strEdge = let (rt,toks) = convertToE (strEdge) in
                  {sEdge|route<-rt,token<-toks}

conNode na = {start|name<-na}

compEdge e0 e1 = e0.route == e1.route
compNode n0 n1 = n0.name  == n1.name

upNode n0 n1 ls = n1::ls
upEdge e0 e1 ls = {e1|token<-e1.token++e0.token}::ls

insE nodes ed ls = let (n0,n1) = ed.route in
                   let (r1,r2) = List.foldl (\el (a1,a2) -> (a1||el.name==n0,a2||el.name==n1)) (False,False) nodes in
                   if r1 && r2 then ed::ls else ls


listPatt comp el (aEl,acc) = case aEl of
  Nothing -> if comp el  then (Just el,acc) else (aEl,el::acc)
  _       -> (aEl,el::acc)

addPatt comp build up  ins state sub = let base = build sub in
                                       let (e,nls) = List.foldl (listPatt (comp base)) (Nothing,[]) state in
                                       case e of 
                                        Nothing -> ins base nls
                                        Just x  ->up base x nls


addNode name  state     = addPatt compNode conNode upNode (\a b->a::b) state name
addEdge strE  state ins = addPatt compEdge conEdge upEdge (\a b ->a::b)   state strE



delEdge str ls = let (rt,token) = convertToE str in
                 let (el,rest)  = List.partition (\el->el.route==rt) ls in
                 let helper =List.filter (\el-> not (List.member el token))  in
                 if [""] == token  then rest 
                 else rest++(List.map (\el->{el|token<-(helper el.token)}) el)

delNode ls str = List.filter (\x->x.name/=str) ls
delENode ls str = List.filter (\x->let (a,b) = x.route in a /=str && b/=str) ls
setFin bool ls mt = List.map (\el-> (if el.name == mt then {el|fin<-bool} 
                                             else el)) ls
setStart ls str =   List.map (\el->{el|start<-(el.name == str)}) ls

updateC action (state,ed) =
    case action of
         InsertNode  v -> (addNode v state,ed)
         DeleteNode v -> (delNode state v ,delENode ed v) 
         DeleteEdge v -> (state,delEdge v ed)
         InsertEdge v -> (state,addEdge v ed state)
         Press ((x,y),pr,ins) -> (if ins && pr then helpy state (( x),( y)) 
                                              else setToF state,ed)  
         SetStart v -> (setStart state v ,ed) 
         SetFinal v -> (setFin True state v,ed)
         RemFinal v -> (setFin False state v,ed)
                                                                



init = ([],[])

runner = foldp updateC ([],[]) mySig
