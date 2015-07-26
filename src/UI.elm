module UI(buttons,Actions(InsertNode,InsertEdge,DeleteEdge,DeleteNode,Press),mySig,aNField,dEField,aEField,dNField,background) where

import Signal exposing (..)
import Graphics.Input exposing (button,hoverable)
import Graphics.Input.Field exposing (..)
import Color exposing (..)
import Graphics.Element exposing (..)
import Mouse exposing (..)
import Graphics.Collage exposing (..)
type Actions  = InsertNode String 
               |InsertEdge String
               |DeleteNode String
               |DeleteEdge String
               |Press      ((Int,Int),Bool,Bool)
               |Yes
               |No
                                                                                                         

--MailBox indicating if mouse is inside a particular element
inside = Signal.mailbox False

--MailBox indicating which button has been press
nodeIMan = Signal.mailbox False
edgeIMan = Signal.mailbox False
edgeDMan = Signal.mailbox False
nodeDMan = Signal.mailbox False

--MailBox for each text field
nodeIF = Signal.mailbox noContent
nodeDF = Signal.mailbox noContent
edgeIF = Signal.mailbox noContent
edgeDF = Signal.mailbox noContent

--buttons 
buttons = [ button (Signal.message  nodeIMan.address True) "Add Node"
          , button (Signal.message edgeIMan.address True) "Add Edge"
          , button (Signal.message edgeDMan.address True) "Remove Edge"
          , button (Signal.message nodeDMan.address True) "Remove Node"]

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


--Used to translate signals into actions
--Somehow
wrap ty a = ty a.string
wrap0 tSig w sSig = sampleOn tSig ((wrap w)<~  sSig)

--Used to create signal that indicates if a mouse is inside the background
hover = hoverable (\a-> if a then Signal.message inside.address True 
                             else  Signal.message inside.address False)

--Background objects will be placed on
background w h = collage (w) (h) [filled red (rect (toFloat w) (toFloat h))] 
                 |> hover




--Consolidates multiple signals into a single signal
pressSig = ((\a b c-> Press (a,b,c)) <~position~isDown~inside.signal)


--Consolidats all signals into the one that will be used by the UI
mySig = mergeMany [wrap0 nodeIMan.signal (InsertNode) nodeIF.signal
                  ,wrap0 nodeDMan.signal (DeleteNode) nodeDF.signal
                  ,wrap0 edgeIMan.signal (InsertEdge) edgeIF.signal
                  ,wrap0 edgeDMan.signal (DeleteEdge) edgeDF.signal
                  ,pressSig]


