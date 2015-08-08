module UI(upButt,testStrSig, testReSig,saveButt,buttons,buttons2,buttons3,buttons4,Actions(..),mySig,fields,fields2,fields3,background) where
--InsertNode,InsertEdge,DeleteEdge,DeleteNode,Press,Save,SetStart,SetFinal,RemFinal,TestRE,TestStr,NewWorld
import Signal exposing (..)
import Graphics.Input exposing (button,hoverable)
import Graphics.Input.Field exposing (..)
import Color exposing (..)
import Graphics.Element exposing (..)
import Mouse exposing (..)
import Graphics.Collage exposing (..)
import Common exposing(Node,Edge)
import Html exposing (input,output,div)
import Html.Attributes exposing (..)
type Actions  = InsertNode String 
               |InsertEdge String
               |DeleteNode String
               |DeleteEdge String
               |Press      ((Int,Int),Bool,Bool)
               |SetStart String
               |SetFinal String
               |RemFinal String
               |TestRE   String
               |TestStr  String
               |NewWorld (List Node,List Edge,Bool)

--MailBox indicating if mouse is inside a particular element
inside = Signal.mailbox False

--MailBox indicating which button has been press
nodeIMan = Signal.mailbox False
edgeIMan = Signal.mailbox False
edgeDMan = Signal.mailbox False
nodeDMan = Signal.mailbox False

addFinnB = Signal.mailbox False
remFinnB = Signal.mailbox False
setStarB = Signal.mailbox False

testReB  = Signal.mailbox False
testStrB = Signal.mailbox False

saveButt = Signal.mailbox False

--MailBox for each text field
nodeIF = Signal.mailbox noContent
nodeDF = Signal.mailbox noContent
edgeIF = Signal.mailbox noContent
edgeDF = Signal.mailbox noContent

addFin = Signal.mailbox noContent
remFin = Signal.mailbox noContent
setStar= Signal.mailbox noContent

testStr= Signal.mailbox noContent
testRe = Signal.mailbox noContent


--buttons 
buttons = [ button (Signal.message  nodeIMan.address True) "Add Node(s)"
          , button (Signal.message nodeDMan.address True) "Remove Node(s)"
          , button (Signal.message edgeIMan.address True) "Add Edge"
          , button (Signal.message edgeDMan.address True) "Remove Edge"
          ]

buttons2= [  button (Signal.message setStarB.address True) "Set Start State" 
          ,  button (Signal.message addFinnB.address True) "Add Final State(s)"
          , button (Signal.message remFinnB.address True) "Remove Final State(s)"
          ]

buttons3=[ 
           button (Signal.message testStrB.address True) "Run on String(s)"
         , button (Signal.message testReB.address True) "Compare to RE"
         ]     


inp = input [type' "file",id "files",name"files[]",multiple True][]
out = output [id "list"][]
upButt =  Html.toElement 0 0 (div [] [inp,out])
buttons4=[button (Signal.message saveButt.address True) "Save"
         ,upButt
         ]


--Makes text field empty when its corresponding button has been pressed
pro sig butt = merge (sampleOn butt (constant noContent)) sig
--Boilerplate required to link a text box and button
boiler box tr mes =  field defaultStyle (Signal.message box.address) mes <~ 
                     pro box.signal tr
--Various text fields 
--Format: a->add
--        d->delete
--        N->node
--        E->Edge
--e.g. aNField = add node text field
aNField = boiler nodeIF nodeIMan.signal "Add Nodes"
dNField = boiler nodeDF nodeDMan.signal "Remove Nodes"
aEField = boiler edgeIF edgeIMan.signal "Add Edges"
dEField = boiler edgeDF edgeDMan.signal "Remove Edges"


finalAd = boiler addFin addFinnB.signal "Add Final State(s)"
finalRm = boiler remFin remFinnB.signal "Remove Final State(s)"
startFd = boiler setStar setStarB.signal "Set Start State"



testReF = boiler testRe testReB.signal "Supply Regular Expression"
testStrF= boiler testStr testStrB.signal "Supply Test String"


fields  = (\a b c d->[a,b,c,d])<~aNField~dNField~aEField~dEField
fields2 =(\a b c ->[a,b,c])<~startFd~finalAd~finalRm
fields3 = (\a b ->[a,b])<~testStrF~testReF

--Used to translate signals into actions
--Somehow
wrap ty a = ty a.string
wrap0 tSig w sSig = sampleOn tSig ((wrap w)<~  sSig)

--Used to create signal that indicates if a mouse is inside the background
hover = hoverable (\a-> if a then Signal.message inside.address True 
                             else  Signal.message inside.address False)

--Background objects will be placed on
background w h = collage (w) (h) [filled yellow (rect (toFloat w) (toFloat h))] 
                 |> hover




--Consolidates multiple signals into a single signal
pressSig = ((\a b c-> Press (a,b,c)) <~position~isDown~inside.signal)

genSig a b = sampleOn a ((\x->x.string)<~b)
testStrSig = genSig testStrB.signal testStr.signal
testReSig = genSig testReB.signal testRe.signal

--Consolidats all signals into the one that will be used by the UI
mySig = mergeMany [wrap0 nodeIMan.signal (InsertNode) nodeIF.signal
                  ,wrap0 nodeDMan.signal (DeleteNode) nodeDF.signal
                  ,wrap0 edgeIMan.signal (InsertEdge) edgeIF.signal
                  ,wrap0 edgeDMan.signal (DeleteEdge) edgeDF.signal
                  ,wrap0 addFinnB.signal  (SetFinal  ) addFin.signal
                  ,wrap0 remFinnB.signal  (RemFinal  ) remFin.signal
                  ,wrap0 setStarB.signal (SetStart  ) setStar.signal
                  ,pressSig]


