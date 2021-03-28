module BusWire

open Fable.React
open Fable.React.Props
open Browser
open Elmish
open Elmish.React
open HelpersOne
open CommonTypes


//------------------------------------------------------------------------//
//------------------------------BusWire Types-----------------------------//
//------------------------------------------------------------------------//


/// type for buswires
/// for demo only. The real wires will
/// connect to Ports - not symbols, where each symbol has
/// a number of ports (see Issie Component and Port types) and have
/// extra informion for highlighting, width, etc.
/// NB - how you define Ports for drawing - whether they correspond to
/// a separadatatype and Id or whether port offsets from
/// component coordinates are held in some other way, is up to groups.
// type HighlightWire = 
//     | Wrong of CommonTypes.Red
//     | Fine of CommonTypes.Black
//     | Hovering of CommonTypes.Blue
//     | Selecting of CommonTypes.Green

type Wire = {
    Id: ConnectionId 
    SrcSymbol: string//CommonTypes.ComponentId // source symbol
    TargetSymbol: string//CommonTypes.ComponentId // target symbol
    SrcPort: string
    TargetPort: string
    Vertices: XYPos List
    Highlighted: bool
    Selected: bool
    BusWidth: int
    IsDragging : bool
    LastDragPos : XYPos List   
    }

type Model = {
    Symbol: Symbol.Model
    Wires: Wire list
    Color: CommonTypes.HighLightColor
    wBB: (XYPos*XYPos) list List
    AutoRouting: bool
    }

//----------------------------Message Type-----------------------------------//

/// Messages to update buswire model
/// These are OK for the demo - but not the correct messages for
/// a production system. In the real system wires must connection
/// to ports, not symbols. In addition there will be other chasnges needed
/// for highlighting, width inference, etc
type Msg =
    | Symbol of Symbol.Msg
    | AddWire of (string * string)
    // | SetColor of CommonTypes.HighLightColor
    | DeleteWire
    | MouseMsg of MouseT
    | ToggleSelect of (CommonTypes.ComponentId list * (Wire * int) list)
    // | Hovering of (CommonTypes.ComponentId list * (Wire * int) list)
    | Dragging of (CommonTypes.ComponentId list * (Wire * int) list) * prevPos: XYPos * currPos: XYPos
    | UpdateBoundingBoxes of (CommonTypes.ComponentId list * (Wire * int) list)
    | SnaptoGrid of (CommonTypes.ComponentId list * (Wire * int) list)
    // | RunBusWidthInference
    // | DraggingList of wId : CommonTypes.ComponentId list  * pagePos: XYPos * prevPagePos: XYPos
    // | EndDragging of wId : CommonTypes.ComponentId
    // | EndDraggingList of wId : CommonTypes.ComponentId list *pagePos:XYPos



type WireRenderProps = {
    key : CommonTypes.ConnectionId
    WireP: Wire
    SrcP: XYPos 
    TgtP: XYPos
    Vertices: XYPos list
    Selected : bool
    Highlighted: bool
    BusWidth: int 
    ColorP: string
    StrokeWidthP: string 
    IsDragging : bool
    LastDragPos : XYPos List 
    // PortInUse : bool
}


// let convertToComp (symbol:Symbol.Symbol): CommonTypes.Component=
//     {
//         Id=string(symbol.Id)
//         Type=symbol.Type
//         Label="Test" //: string // All components have a label that may be empty.
//         InputPorts=symbol.InputPorts
//         OutputPorts=symbol.OutputPorts
//         X = int(symbol.Pos.X)
//         Y = int(symbol.Pos.Y)
//         H = int(symbol.H)
//         W = int(symbol.W)
//     }

// let findsymbol wire model=
//     (List.find (fun (sym:Symbol.Symbol) -> string(sym.Id)=wire.SrcSymbol) model.Symbol.Symbols)

// let convertToConnect (wire:Wire) model: CommonTypes.Connection=
//     {
//         Id=string(wire.Id)
//         Source= List.find (fun port -> port.Id=wire.TargetPort) (findsymbol wire model).InputPorts
//         Target=(List.find (fun port -> port.Id=wire.SrcPort) (List.find (fun (sym:Symbol.Symbol) -> string(sym.Id)=wire.TargetSymbol) model.Symbol.Symbols).OutputPorts)
//         Vertices=List.map (fun xypos -> (xypos.X,xypos.Y)) wire.Vertices
//     }

// let private runBusWidthInference model =
//     let listofSymbols : CommonTypes.Component list=
//         List.fold (fun state (sym:Symbol.Symbol) -> state@[(convertToComp sym)]) [] model.Symbol.Symbols
    
//     let listofWires : CommonTypes.Connection list=
//         List.fold (fun state wire -> state@[convertToConnect wire model]) [] model.Wires

//     (listofSymbols,listofWires)
//     |> BusWidthInferer.inferConnectionsWidth
//     |> function
//     | Error e ->
//             // model
//             // TODO: this makes the content of the model.Higlighted inconsistent.
//             // Need to dispatch SetHighlighted (can do by using mkProgram).
//             // e.ConnectionsAffected
//             // |> List.iter (fun connection -> model.Diagram.HighlightConnection c "red")
//             let updatedWires=
//                 List.map (fun (wire:Wire) -> if List.exists (fun connid -> connid=wire.Id) e.ConnectionsAffected
//                                                      then {wire with Highlighted = true}
//                                                      else {wire with Highlighted = false} ) model.Wires
//             printfn "%A" e.Msg
//             {model with Wires=updatedWires}
//             // // Display notification withupdatedWires error message.
//             // { model with 
//             //     Notifications =
//             //         { model.Notifications with 
//             //             FromDiagram = Some <| errorNotification e.Msg CloseDiagramNotification} }
//     | Ok connsWidth ->
//             // repaintConnections model connsWidth
//             // repaintBusComponents model connsWidth state
//             // Close the notification if all is good.
//             model

let posDiff (a:XYPos) (b:XYPos) =
    {X=a.X-b.X; Y=a.Y-b.Y}

let posAdd (a:XYPos) (b:XYPos) =
    {X=a.X+b.X; Y=a.Y+b.Y}

let posOf x y = {X=x;Y=y}

/// look up wire in WireModel
let wire (wModel: Model)(wId: CommonTypes.ConnectionId): Option<Wire> =
    wModel.Wires
    |> List.tryPick (function {Id = wId} as x -> Some x) 

let midpoint (startPoint:XYPos) (endPoint:XYPos) = 
    if startPoint.Y = endPoint.Y then 
        endPoint.X 
    else
        ((endPoint.X - startPoint.X)/2.0 + startPoint.X)
        

let rec autoroute (isHorizontal: bool) (nextPort: XYPos) (startPort:XYPos) (endPort: XYPos) (model:Model) (lastPos: XYPos) (count:int)=
    let boundingBoxSearchS (Port: XYPos) =
        List.filter (fun (BB:(XYPos*XYPos)) -> (Port.X >= (fst BB).X && Port.X <= (snd BB).X) || (Port.X <= (fst BB).X && Port.X >= (snd BB).X)) model.Symbol.SymBBoxes// go through to find all bboxlists in symbol with correct x range 
        |> List.filter ( fun BB -> (Port.Y >= (fst BB).Y && Port.Y <= (snd BB).Y) || (Port.Y <= (fst BB).Y && Port.Y >= (snd BB).Y) ) 
    let checkBBHorizontal = List.filter (fun (sym:(XYPos*XYPos)) -> (nextPort.X >  ((fst sym).X)) && (lastPos.X < (fst sym).X)) model.Symbol.SymBBoxes
                            |> List.filter ( fun sym -> ((fst sym).Y < nextPort.Y) && ((snd sym).Y > nextPort.Y))// we will change this to 
                            |> List.filter (fun sym -> ([sym] <> (boundingBoxSearchS startPort)) && ([sym] <> (boundingBoxSearchS endPort)))
                            //|> List.filter (fun sym -> (startPort.X <= fst sym.[0] && endPort.X >= fst sym.[0]))
    let checkBBVertical = List.filter (fun (sym:(XYPos*XYPos)) -> (nextPort.Y >  (snd sym).Y) && (lastPos.Y < (fst sym).Y)) model.Symbol.SymBBoxes
                          |> List.filter ( fun sym -> ((fst sym).X < nextPort.X) && ((snd sym).X > nextPort.X))
                          |> List.filter (fun sym -> ([sym] <> (boundingBoxSearchS startPort)) && ([sym] <> (boundingBoxSearchS endPort)))// we will change this to 
                          |> List.filter (fun sym -> (if startPort.Y <=endPort.Y then (startPort.Y <= (snd sym).Y && endPort.Y >= (snd sym).Y) || (startPort.Y <= (fst sym).Y && endPort.Y >= (fst sym).Y) else (startPort.Y <= (snd sym).Y && endPort.Y >= (snd sym).Y) || (startPort.Y <= (fst sym).Y && endPort.Y >= (fst sym).Y) ))
    let checkBBY = List.filter (fun (BB:(XYPos*XYPos)) -> (nextPort.Y > (fst BB).Y) && nextPort.Y < ((snd BB).Y)) model.Symbol.SymBBoxes
                   |> List.filter (fun (BB:(XYPos*XYPos)) -> (nextPort.X < (snd BB).X) && endPort.X > ((snd BB).X))
                   |> List.filter (fun sym -> ([sym] <> (boundingBoxSearchS startPort)))
    let checkBBX = List.filter (fun (BB:(XYPos*XYPos)) -> (nextPort.X > (fst BB).X) && nextPort.X < (snd BB).X) model.Symbol.SymBBoxes
                   |> List.filter (fun (BB:(XYPos*XYPos)) -> (nextPort.Y >= (fst BB).Y && endPort.Y <= (fst BB).Y) || (nextPort.Y <= (fst BB).Y && endPort.Y >= (fst BB).Y)) 
                   |> List.filter (fun sym -> ([sym] <> (boundingBoxSearchS startPort)))

    //printfn "bbhor %A checkbbx %A checkBBy %A np %A sp %A ep %A lp %A bbox %A" checkBBHorizontal  checkBBX checkBBY nextPort startPort endPort lastPos model.Symbol.sBB
    //printfn "bbver %A veorho %A" checkBBVertical isHorizontal
    if count = 20
    then []
    else
        if nextPort = endPort then 
            []
            else 
                match isHorizontal with 
                |true -> match checkBBHorizontal with 
                              |[BBIntersect] -> (*printfn "First Horizontal with intersect %A" BBIntersect*)
                                                if (nextPort.Y - (fst BBIntersect).Y) > ((snd BBIntersect).Y- nextPort.Y)
                                                then {X = (fst BBIntersect).X - 5.; Y = nextPort.Y} :: autoroute (not isHorizontal) {X = (fst BBIntersect).X - 5.; Y = (snd BBIntersect).Y + 10.} startPort endPort model {X = (fst BBIntersect).X - 5.; Y = nextPort.Y} (count+1)
                                                else {X = (fst BBIntersect).X - 5.; Y = nextPort.Y} :: autoroute (not isHorizontal) {X = (fst BBIntersect).X - 5.; Y = (fst BBIntersect).Y - 10.} startPort endPort model {X = (fst BBIntersect).X - 5.; Y = nextPort.Y} (count+1)
                              |[] ->        let minBBoxHList = List.sortByDescending (fun (bb:(XYPos*XYPos)) -> abs((fst bb).X - nextPort.X)) checkBBX  
                                            let listcalc = List.map (fun (bb:(XYPos*XYPos)) -> abs((fst bb).X - nextPort.X)) checkBBX 
                                            //printfn "Horizontal vertical hit %A listorder %A" minBBoxHList listcalc
                                            if minBBoxHList <> [] && ([List.last minBBoxHList] <> (boundingBoxSearchS endPort))
                                               then let minBBoxH = minBBoxHList |> List.last
                                                    //printfn "Horizontal Vertical Hit List Head %A" minBBoxH
                                                    //printfn "hola %A hola2 %A" ((fst minBBoxH.[2]) - nextPort.X) (nextPort.X - (fst minBBoxH.[0]))
                                                    if ((snd minBBoxH).X - nextPort.X) > (nextPort.X - (fst minBBoxH).X)
                                                    then if boundingBoxSearchS {X = (fst minBBoxH).X; Y = endPort.Y} = [] 
                                                         then {X=(fst minBBoxH).X; Y=nextPort.Y} :: autoroute (not isHorizontal) {X = (fst minBBoxH).X; Y = endPort.Y} startPort endPort model {X=(fst minBBoxH).X; Y=nextPort.Y} (count+1)
                                                         else {X=(snd minBBoxH).X; Y=nextPort.Y} :: autoroute (not isHorizontal) {X = (snd minBBoxH).X; Y = endPort.Y} startPort endPort model {X=(snd minBBoxH).X; Y=nextPort.Y} (count+1)
                                                    else {X=((snd minBBoxH).X); Y=nextPort.Y} :: autoroute (not isHorizontal) {X = ((snd minBBoxH).X); Y = endPort.Y} startPort endPort model {X=(snd minBBoxH).X; Y=nextPort.Y} (count+1)
                                               else 
                                                    nextPort :: autoroute (not isHorizontal) {X=nextPort.X; Y = endPort.Y } startPort endPort model nextPort (count+1)
                  |false ->   match checkBBVertical with 
                              |[BBIntersect] -> (*printfn "Vertical Intersect %A" BBIntersect*)
                                                if endPort.Y > startPort.Y
                                                then {X = nextPort.X; Y = ((fst BBIntersect).Y-5.)} :: autoroute (not isHorizontal) {X = (midpoint {X = nextPort.X; Y = (fst BBIntersect).Y-5.} endPort); Y =(fst BBIntersect).Y-5.} startPort endPort model {X = nextPort.X; Y = (fst BBIntersect).Y-5.} (count+1)
                                                else {X = nextPort.X; Y = ((snd BBIntersect).Y-5.)} :: autoroute (not isHorizontal) {X = (midpoint {X = nextPort.X; Y = (snd BBIntersect).Y-5.} endPort); Y =(snd BBIntersect).Y-5.} startPort endPort model {X = nextPort.X; Y = (snd BBIntersect).Y-5.} (count+1)
                              |[] -> let minBBoxList = List.sortByDescending (fun (bb:(XYPos*XYPos)) -> abs((fst bb).X - nextPort.X)) checkBBY 
                                     //printfn "Vertical Horizontal Hit List Head %A" minBBoxList
                                     if (minBBoxList <> []) && ([List.last minBBoxList] <> (boundingBoxSearchS endPort))
                                     then let minBBox = List.last minBBoxList
                                          if ((snd minBBox).Y - nextPort.Y) > (nextPort.Y - ((fst minBBox).Y))
                                          then if boundingBoxSearchS {X = (midpoint nextPort endPort); Y=(((fst minBBox).Y))} = [] 
                                               then {X=nextPort.X; Y=((fst minBBox).Y)} :: autoroute (not isHorizontal) {X = (midpoint nextPort endPort); Y=((fst minBBox).Y)} startPort endPort model {X=nextPort.X; Y=((fst minBBox).Y)} (count+1)
                                               else {X=nextPort.X; Y=((snd minBBox).Y)} :: autoroute (not isHorizontal) {X = (midpoint nextPort endPort); Y=((snd minBBox).Y)} startPort endPort model {X=nextPort.X; Y=((snd minBBox).Y)} (count+1)
                                          else 
                                               if boundingBoxSearchS {X = (midpoint nextPort endPort); Y=((snd minBBox).Y)} = [] 
                                               then {X=nextPort.X; Y=((snd minBBox).Y)} :: autoroute (not isHorizontal) {X = (midpoint nextPort endPort); Y=((snd minBBox).Y)} startPort endPort model {X=nextPort.X; Y=((snd minBBox).Y)} (count+1)
                                               else {X=nextPort.X; Y=((fst minBBox).Y)} :: autoroute (not isHorizontal) {X = (midpoint nextPort endPort); Y=((fst minBBox).Y)} startPort endPort model {X=nextPort.X; Y=((fst minBBox).Y)} (count+1)
                                     else 
                                             nextPort :: autoroute (not isHorizontal) {X = (midpoint nextPort endPort); Y = nextPort.Y} startPort endPort model nextPort (count+1)
                              

let rec autoroute5 (isHorizontal: bool) (nextPort: XYPos) (startPort:XYPos) (endPort: XYPos) (model:Model)(lastPos:XYPos) (count:int)=
    //let boundingBoxSearchS Port=
    //    List.filter (fun (x:(float*float) list) -> (Port.X >= fst x.[0] && Port.X <= fst x.[1]) || (Port.X <= fst x.[0] && Port.X >= fst x.[1])) model.Symbol.SymBBoxes// go through to find all bboxlists in symbol with correct x range 
    //    |> List.filter ( fun x -> (Port.Y >= snd x.[0] && Port.Y <= snd x.[3]) || (Port.Y <= snd x.[0] && Port.Y >= snd x.[3]) ) 
    //let checkBBHorizontal = List.filter (fun (sym:(float*float) list) -> nextPort.X >  (fst sym.[0])) model.Symbol.SymBBoxes
    //                        |> List.filter ( fun sym -> (snd sym.[0]) < nextPort.Y && (snd sym.[2]) > nextPort.Y)// we will change this to 
    //let checkBBVertical = List.filter (fun (sym:(float*float) list) -> nextPort.Y >  (snd sym.[0])) model.Symbol.SymBBoxes
    //                      |> List.filter ( fun sym -> (fst sym.[0]) < nextPort.X && (fst sym.[2]) > nextPort.X)
    //let checkBBY = List.filter (fun (x:(float*float) list) -> (nextPort.Y >= snd x.[0] && nextPort.Y <= snd x.[2])) model.Symbol.sBB
                   
    //let checkBBX = List.filter (fun (x:(float*float) list) -> (nextPort.X >= fst x.[0] && nextPort.X <= fst x.[2])) model.Symbol.sBB

    ////printfn "bbhor %A bbvert %A checkBB %A np %A sp %A ep %A bbox %A" checkBBHorizontal  checkBBX checkBBY nextPort startPort endPort model.Symbol.sBB
    //if nextPort = endPort then 
    //    [nextPort]
    //    else 
    //        match isHorizontal with 
    //        |true -> if nextPort.Y = startPort.Y 
    //                 then match checkBBHorizontal with 
    //                      |[BBIntersect] when [BBIntersect] <> boundingBoxSearchS startPort -> {X = (fst BBIntersect.[0]) - 5.; Y = nextPort.Y} :: autoroute5 (not isHorizontal) {X = (fst BBIntersect.[0]) - 5.; Y = (snd BBIntersect.[0]) - 10.} startPort endPort model
    //                      |[] -> let minBBox2h = List.sortByDescending (fun (bb:(float*float) list) -> abs((fst bb.[0]) - nextPort.X)) checkBBX  
    //                             //printfn "pot %A" minBBox2h
    //                             if minBBox2h <> []
    //                                then let minBBoxh = minBBox2h |> List.last
    //                                     if ((fst minBBoxh.[2]) - nextPort.X) > (nextPort.X - (fst minBBoxh.[0]))
    //                                     then {X=(fst minBBoxh.[0]); Y=nextPort.Y} :: autoroute5 (not isHorizontal) {X = (fst minBBoxh.[0]); Y = endPort.Y} startPort endPort model
    //                                     else {X=(fst minBBoxh.[2]); Y=nextPort.Y} :: autoroute5 (not isHorizontal) {X = (fst minBBoxh.[2]); Y = endPort.Y} startPort endPort model
    //                                else 
    //                                        nextPort :: autoroute5 (not isHorizontal) {X=nextPort.X; Y = endPort.Y } startPort endPort model                     
    //                 else 
    //                      let minBBox2h = List.sortByDescending (fun (bb:(float*float) list) -> abs((fst bb.[0]) - nextPort.X)) checkBBX  
    //                      //printfn "pot %A" minBBox2h
    //                      if minBBox2h <> []
    //                         then let minBBoxh = minBBox2h |> List.last
    //                              if ((fst minBBoxh.[2]) - nextPort.X) > (nextPort.X - (fst minBBoxh.[0]))
    //                              then {X=(fst minBBoxh.[0]); Y=nextPort.Y} :: autoroute5 (not isHorizontal) {X = (fst minBBoxh.[0]); Y = endPort.Y} startPort endPort model
    //                              else {X=(fst minBBoxh.[2]); Y=nextPort.Y} :: autoroute5 (not isHorizontal) {X = (fst minBBoxh.[2]); Y = endPort.Y} startPort endPort model
    //                         else 
    //                              nextPort :: autoroute5 (not isHorizontal) {X=nextPort.X; Y = endPort.Y } startPort endPort model

    //        |false -> if nextPort.Y = startPort.Y
    //                  then match checkBBVertical with 
    //                        |[BBIntersect] when [BBIntersect] = boundingBoxSearchS startPort -> let minBBox2 = List.sortByDescending (fun (bb:(float*float) list) -> abs((fst bb.[0]) - nextPort.X)) checkBBY  
    //                                                                                            if minBBox2 <> []
    //                                                                                            then let minBBox = minBBox2 |> List.last
    //                                                                                                 if ((snd minBBox.[2]) - nextPort.X) > (nextPort.X - (snd minBBox.[0]))
    //                                                                                                 then {X=nextPort.X; Y=((snd minBBox.[0]))} :: autoroute5 (not isHorizontal) {X = (midpoint nextPort endPort); Y=((snd minBBox.[0]))} startPort endPort model
    //                                                                                                 else {X=nextPort.X; Y=((snd minBBox.[2]))} :: autoroute5 (not isHorizontal) {X = (midpoint nextPort endPort); Y=((snd minBBox.[2]))} startPort endPort model
    //                                                                                            else 
    //                                                                                                 nextPort :: autoroute5 (not isHorizontal) {X = (midpoint nextPort endPort); Y = nextPort.Y} startPort endPort model
    //                        |[BBIntersect::BBOther] -> if startPort.Y >= endPort.Y
    //                                                        then {X = nextPort.X; Y = (snd BBOther.[0])} :: autoroute5 (not isHorizontal) {X = (midpoint nextPort endPort); Y =(snd BBOther.[2])} startPort endPort model
    //                                                        else {X = nextPort.X; Y = (snd BBOther.[0])} :: autoroute5 (not isHorizontal) {X = (midpoint nextPort endPort); Y =nextPort.X} startPort endPort model

    //                        |[] -> let minBBox2 = List.sortByDescending (fun (bb:(float*float) list) -> abs((fst bb.[0]) - nextPort.X)) checkBBY  
    //                               if minBBox2 <> []
    //                               then let minBBox = minBBox2 |> List.last
    //                                    if ((snd minBBox.[2]) - nextPort.X) > (nextPort.X - (snd minBBox.[0]))
    //                                    then {X=nextPort.X; Y=((snd minBBox.[0]))} :: autoroute5 (not isHorizontal) {X = (midpoint nextPort endPort); Y=((snd minBBox.[0]))} startPort endPort model
    //                                    else {X=nextPort.X; Y=((snd minBBox.[2]))} :: autoroute5 (not isHorizontal) {X = (midpoint nextPort endPort); Y=((snd minBBox.[2]))} startPort endPort model
    //                               else 
    //                                       nextPort :: autoroute5 (not isHorizontal) {X = (midpoint nextPort endPort); Y = nextPort.Y} startPort endPort model
    //                        else
    //                            let minBBox2 = List.sortByDescending (fun (bb:(float*float) list) -> abs((fst bb.[0]) - nextPort.X)) checkBBY  
    //                            if minBBox2 <> []
    //                            then let minBBox = minBBox2 |> List.last
    //                                 if ((snd minBBox.[2]) - nextPort.X) > (nextPort.X - (snd minBBox.[0]))
    //                                 then {X=nextPort.X; Y=((snd minBBox.[0]))} :: autoroute5 (not isHorizontal) {X = (midpoint nextPort endPort); Y=((snd minBBox.[0]))} startPort endPort model
    //                                 else {X=nextPort.X; Y=((snd minBBox.[2]))} :: autoroute5 (not isHorizontal) {X = (midpoint nextPort endPort); Y=((snd minBBox.[2]))} startPort endPort model
    //                            else 
    //                                    nextPort :: autoroute5 (not isHorizontal) {X = (midpoint nextPort endPort); Y = nextPort.Y} startPort endPort model
    let boundingBoxSearchS (Port: XYPos)  =
        List.filter (fun (BB:(XYPos*XYPos)) -> (Port.X >= (fst BB).X && Port.X <= (snd BB).X) || (Port.X <= (fst BB).X && Port.X >= (snd BB).X)) model.Symbol.SymBBoxes// go through to find all bboxlists in symbol with correct x range 
        |> List.filter ( fun BB -> (Port.Y >= (fst BB).Y && Port.Y <= (snd BB).Y) || (Port.Y <= (fst BB).Y && Port.Y >= (snd BB).Y) ) 
    let checkBBHorizontal = List.filter (fun (sym:(XYPos*XYPos)) -> (nextPort.X > (fst sym).X) && (lastPos.X < (fst sym).X)) model.Symbol.SymBBoxes
                            |> List.filter ( fun sym -> ((fst sym).Y) < nextPort.Y && ((snd sym).Y) > nextPort.Y)// we will change this to 
                            |> List.filter (fun sym -> ([sym] <> (boundingBoxSearchS startPort)) && ([sym] <> (boundingBoxSearchS endPort)))
                            //|> List.filter (fun sym -> (startPort.X <= fst sym.[0] && endPort.X >= fst sym.[0]))
    let checkBBVertical = List.filter (fun (sym:(XYPos*XYPos)) -> (nextPort.Y >  ((fst sym).Y)) && (lastPos.Y < ((fst sym).Y)))model.Symbol.SymBBoxes
                          |> List.filter ( fun sym -> ((fst sym).X) < nextPort.X && ((snd sym).X) > nextPort.X)
                          |> List.filter (fun sym -> ([sym] <> (boundingBoxSearchS startPort)) && ([sym] <> (boundingBoxSearchS endPort)))// we will change this to 
                          |> List.filter (fun sym -> (if startPort.Y <=endPort.Y then (startPort.Y <= (snd sym).Y && endPort.Y >= (snd sym).Y) || (startPort.Y <= (fst sym).Y && endPort.Y >= (fst sym).Y) else (startPort.Y <= (snd sym).Y && endPort.Y >= (snd sym).Y) || (startPort.Y <= (fst sym).Y && endPort.Y >= (fst sym).Y) ))
    let checkBBY = List.filter (fun (BB:(XYPos*XYPos)) -> (nextPort.Y > (fst BB).Y && nextPort.Y < (snd BB).Y)) model.Symbol.SymBBoxes
                   |> List.filter (fun (BB:(XYPos*XYPos)) -> (nextPort.X < (snd BB).X && endPort.X > (snd BB).X))
                   |> List.filter (fun sym -> ([sym] <> (boundingBoxSearchS startPort)))
    let checkBBX = List.filter (fun (x:(XYPos*XYPos)) -> (nextPort.X > (fst x).X && nextPort.X < (snd x).X)) model.Symbol.SymBBoxes
                   |> List.filter (fun (x:(XYPos*XYPos)) -> (nextPort.Y >= (snd x).Y && endPort.Y <= (snd x).Y) || (nextPort.Y <= (fst x).Y && endPort.Y >= (fst x).Y)) 
                   |> List.filter (fun sym -> ([sym] <> (boundingBoxSearchS startPort)))

    printfn "bbhor %A checkbbx %A checkBBy %A np %A sp %A ep %A lp %A bbox %A" checkBBHorizontal  checkBBX checkBBY nextPort startPort endPort lastPos model.Symbol.SymBBoxes
    printfn "bbver %A veorho %A" checkBBVertical isHorizontal
    if count = 20
    then []
    else
        if nextPort = endPort then 
            [nextPort]
            else 
                match isHorizontal with 
                |true -> match checkBBHorizontal with 
                              |[BBIntersect] -> printfn "First Horizontal with intersect %A" BBIntersect
                                                if (nextPort.Y - ((fst BBIntersect).Y)) > (((snd BBIntersect).Y)- nextPort.Y)
                                                then {X = ((fst BBIntersect).X)- 5.; Y = nextPort.Y} :: autoroute5 (not isHorizontal) {X = ((fst BBIntersect).X)- 5.; Y = ((snd BBIntersect).Y)+ 10.} startPort endPort model {X = ((fst BBIntersect).X)- 5.; Y = nextPort.Y} (count+1)
                                                else {X = ((fst BBIntersect).X)- 5.; Y = nextPort.Y} :: autoroute5 (not isHorizontal) {X = ((fst BBIntersect).X)- 5.; Y = ((fst BBIntersect).Y)- 10.} startPort endPort model {X = ((fst BBIntersect).X)- 5.; Y = nextPort.Y} (count+1)
                              |[] ->        let minBBoxHList = List.sortByDescending (fun (bb:(XYPos*XYPos)) -> abs(((fst bb).X) - nextPort.X)) checkBBX  
                                            let listcalc = List.map (fun (bb:(XYPos*XYPos)) -> abs(((fst bb).X) - nextPort.X)) checkBBX 
                                            printfn "Horizontal vertical hit %A listorder %A" minBBoxHList listcalc
                                            if minBBoxHList <> [] && ([List.last minBBoxHList] <> (boundingBoxSearchS endPort))
                                               then let minBBoxH = minBBoxHList |> List.last
                                                    printfn "Horizontal Vertical Hit List Head %A" minBBoxH
                                                    printfn "hola %A hola2 %A" (((snd minBBoxH).X) - nextPort.X) (nextPort.X - ((fst minBBoxH).X))
                                                    if (((snd minBBoxH).X) - nextPort.X) > (nextPort.X - ((fst minBBoxH).X))
                                                    then if boundingBoxSearchS {X = ((fst minBBoxH).X); Y = endPort.Y} = [] 
                                                         then {X=((fst minBBoxH).X); Y=nextPort.Y} :: autoroute5 (not isHorizontal) {X = ((fst minBBoxH).X); Y = endPort.Y} startPort endPort model {X=((fst minBBoxH).X); Y=nextPort.Y} (count+1)
                                                         else {X=((snd minBBoxH).X); Y=nextPort.Y} :: autoroute5 (not isHorizontal) {X = ((snd minBBoxH).X); Y = endPort.Y} startPort endPort model {X=((snd minBBoxH).X); Y=nextPort.Y} (count+1)
                                                    else {X=((snd minBBoxH).X); Y=nextPort.Y} :: autoroute5 (not isHorizontal) {X = ((snd minBBoxH).X); Y = endPort.Y} startPort endPort model {X=((snd minBBoxH).X); Y=nextPort.Y} (count+1)
                                               else 
                                                    nextPort :: autoroute5 (not isHorizontal) {X=nextPort.X; Y = endPort.Y } startPort endPort model nextPort (count+1)
                  |false ->   match checkBBVertical with 
                              |[BBIntersect] -> printfn "Vertical Intersect %A" BBIntersect
                                                if endPort.Y > startPort.Y
                                                then {X = nextPort.X; Y = ((fst BBIntersect).Y)-5.} :: autoroute5 (not isHorizontal) {X = (midpoint {X = nextPort.X; Y = ((fst BBIntersect).Y)-5.} endPort); Y =((fst BBIntersect).Y)-5.} startPort endPort model {X = nextPort.X; Y = ((fst BBIntersect).Y)-5.} (count+1)
                                                else {X = nextPort.X; Y = ((snd BBIntersect).Y)-5.} :: autoroute5 (not isHorizontal) {X = (midpoint {X = nextPort.X; Y = ((snd BBIntersect).Y)-5.} endPort); Y =((snd BBIntersect).Y)-5.} startPort endPort model {X = nextPort.X; Y = ((snd BBIntersect).Y)-5.} (count+1)
                              |[] -> let minBBoxList = List.sortByDescending (fun (bb:(XYPos*XYPos)) -> abs(((fst bb).X) - nextPort.X)) checkBBY 
                                     printfn "Vertical Horizontal Hit List Head %A" minBBoxList
                                     if (minBBoxList <> []) && ([List.last minBBoxList] <> (boundingBoxSearchS endPort))
                                     then let minBBox = List.last minBBoxList
                                          if (((snd minBBox).Y) - nextPort.Y) > (nextPort.Y - ((fst minBBox).Y))
                                          then if boundingBoxSearchS {X = (midpoint nextPort endPort); Y=((fst minBBox).Y)} = [] 
                                               then {X=nextPort.X; Y=(((fst minBBox).Y))} :: autoroute5 (not isHorizontal) {X = (midpoint nextPort endPort); Y=(((fst minBBox).Y))} startPort endPort model {X=nextPort.X; Y=(((fst minBBox).Y))} (count+1)
                                               else {X=nextPort.X; Y=(((snd minBBox).Y))} :: autoroute5 (not isHorizontal) {X = (midpoint nextPort endPort); Y=(((snd minBBox).Y))} startPort endPort model {X=nextPort.X; Y=(((snd minBBox).Y))} (count+1)
                                          else 
                                               if boundingBoxSearchS {X = (midpoint nextPort endPort); Y=((snd minBBox).Y)} = [] 
                                               then {X=nextPort.X; Y=((snd minBBox).Y)} :: autoroute5 (not isHorizontal) {X = (midpoint nextPort endPort); Y=(((snd minBBox).Y))} startPort endPort model {X=nextPort.X; Y=(((snd minBBox).Y))} (count+1)
                                               else {X=nextPort.X; Y=(((fst minBBox).Y))} :: autoroute5 (not isHorizontal) {X = (midpoint nextPort endPort); Y=(((fst minBBox).Y))} startPort endPort model {X=nextPort.X; Y=(((fst minBBox).Y))} (count+1)
                                     else 
                                             nextPort :: autoroute5 (not isHorizontal) {X = (midpoint nextPort endPort); Y = nextPort.Y} startPort endPort model nextPort (count+1)

let newWireRoute  (tgtPort:XYPos) (sourcePort:XYPos) (model:Model) : XYPos list =
    if sourcePort.X < tgtPort.X 
    then 
        let newtgt = {X=tgtPort.X - 15.; Y = tgtPort.Y}
        let calcMid = midpoint sourcePort newtgt
        let final = (autoroute true {X=calcMid; Y = sourcePort.Y} sourcePort newtgt model sourcePort 0) @ [tgtPort]
                    |> List.append [sourcePort]
        printfn "add 1 %A" final 
        final 
            
        
    else 
        let calcMidSwap = midpoint  tgtPort sourcePort
        List.rev (autoroute5 false {X= tgtPort.X - 15. ; Y = sourcePort.Y} {X= tgtPort.X - 15.; Y= tgtPort.Y} {X= sourcePort.X + 15.; Y = sourcePort.Y} model {X= tgtPort.X - 15.; Y= tgtPort.Y} 0) @ [{X= tgtPort.X - 15.; Y= tgtPort.Y};tgtPort]
        |> List.append [sourcePort; {X= sourcePort.X + 15.; Y = sourcePort.Y}]

/// newWireRoute calculates the wire route between 2 port positions. It returns a list of XY Positions, which are in the 
/// form of vertices, including the source port and target port positions. It calculates how many segments the wire
/// will have. 
//let newWireRoute  (targetPort: XYPos) (sourcePort: XYPos) : XYPos list =
//    let threeSegWire : XYPos list =
//        let xDifference = targetPort.X - sourcePort.X
//        let midpoint = float (xDifference/2.0)
//        let midX = sourcePort.X + midpoint
//        [{X= sourcePort.X ; Y= sourcePort.Y}; {X= midX ; Y=sourcePort.Y}; {X= midX; Y= targetPort.Y}; {X=targetPort.X; Y= targetPort.Y}]
    
//    let fiveSegWire : XYPos list = 
//        let xDifference = targetPort.X - sourcePort.X
//        let yDifference = targetPort.Y - sourcePort.Y
//        let xMidpoint = float (xDifference/2.0)
//        let yMidpoint = float (yDifference/2.0)
//        let midX = sourcePort.X + xMidpoint
//        let midY = sourcePort.Y + yMidpoint
//        let offset = 20.0
//        [{X=sourcePort.X; Y=sourcePort.Y};{X=(sourcePort.X+offset); Y= sourcePort.Y};{X=(sourcePort.X+offset); Y= midY};{X= float (targetPort.X - offset); Y= midY};{X=float(targetPort.X - offset); Y= targetPort.Y}; {X= targetPort.X ; Y=targetPort.Y}]
    
//    if sourcePort.X + 10.< targetPort.X then threeSegWire 
//    else fiveSegWire

/// A useful function to convert the list of vertices into a list of segments. A list of vertices is an XYPos list, whereas a list of
/// segments is a list in which each element is XYPos*XYPos (to represent start and end points).
let segmentList (vertexList: XYPos list) : (XYPos * XYPos) list= vertexList |> List.pairwise

/// Finding the bounding box for a given wire (ie, list of vertices). The padding determines how large you would like the bounding
/// box to be. Returns XYPos*XYPos list, with both the XYPos being the diagonal points in a bounding box. 
let wireBoundingBoxes (verticesList: XYPos list) =
    let padding = 10.0
    let rightX (segment: XYPos list) = segment |> List.sortBy (fun v -> v.X)
    let leftX (segment: XYPos list) = segment |> List.sortByDescending (fun v -> v.X)
    let topY (segment: XYPos list) = segment |> List.sortByDescending (fun v -> v.Y)
    let bottomY (segment: XYPos list) = segment |> List.sortBy (fun v -> v.Y)
    let findBox (startVertex: XYPos) (endVertex: XYPos) : (XYPos*XYPos)=
        if startVertex.X = endVertex.X then 
            let maxY = (bottomY [startVertex;endVertex]).Head
            let minY = (topY [startVertex; endVertex]).Head
            {minY with X=startVertex.X - padding}, {maxY with X=startVertex.X + padding}
        else 
            let maxX = (rightX[startVertex ; endVertex]).Head 
            let minX = (leftX[startVertex; endVertex]).Head
            {minX with Y=startVertex.Y - padding}, {maxX with Y=startVertex.Y + padding}
    let makeSegList = segmentList verticesList 
    makeSegList 
    |> List.map (fun x -> findBox (fst x) (snd x))


/// singleWireView maps the list of vertices to a list of segments, then draws each individual line by passing in the XY positions to singularLine.
/// We are able to view the wire through this function, as well as the bus width legend, and a change in colour if Selected, or if the wire has 
/// a larger bus width. 
let singleWireView = 
    FunctionComponent.Of(
        fun (props: WireRenderProps) ->
            
                
            let singularLine (pairVertices: XYPos*XYPos) = 
                let fst, snd = pairVertices
              
                line [
                    X1 fst.X // accessing the x value of SrcP since its of type XYPos
                    Y1 fst.Y
                    X2 snd.X
                    Y2 snd.Y 
                    
                    
                    SVGAttr.Stroke props.ColorP //(if props.BusWidth = 1 then props.ColorP elif props.Selected = true then "yellow" else "darkorchid")
                    SVGAttr.StrokeWidth (if props.BusWidth = 1 then props.StrokeWidthP else "5px")
                    SVGAttr.StrokeLinecap "round"  ] []

            let busWidthLegend =
                let legendPos = posAdd props.Vertices.Head {X=10.;Y=5.}
                text [
                    X legendPos.X
                    Y legendPos.Y
                    Style [
                        FontSize "10px"
                        FontWeight "Bold"
                        Fill "Black"
                        TextAnchor "middle"
                        DominantBaseline "hanging"
                        ]
                ] [str <| sprintf "%i" props.BusWidth]

            //let multipleWireCircle =
                
            //    circle [
            //        X legendPos.X
            //        Y legendPos.Y
            //        Style [
            //            FontSize "10px"
            //            FontWeight "Bold"
            //            Fill "Black"
            //            TextAnchor "middle"
            //            DominantBaseline "hanging"
            //            ]
            //    ] [str <| sprintf "%i" props.BusWidth]
                
            let singleSeg = segmentList props.Vertices // takes in the list of vertices that make up a wire and maps these to segments. 
            let segmentsIntoLine = singleSeg |> List.map singularLine 
            (busWidthLegend:: segmentsIntoLine)
            |> ofList 
                     
         
                    )

/// The view function takes every wire in the model, and its attributes, and maps this to singleWireView. The helper
/// function convertIdToPort is not my code, but a teammates. 
let view (model:Model) (dispatch: Dispatch<Msg>)=
    let convertIdToPort inOut (id:string) =
        match inOut with 
        |1 -> List.collect (fun (x:Symbol.Symbol) -> (List.tryFind (fun (y:CommonTypes.Port) -> y.Id = id) x.InputPorts) |> function |Some a -> [a] |None -> []) model.Symbol.Symbols
              |>List.head
        |0 -> List.collect (fun (x:Symbol.Symbol) -> (List.tryFind (fun (y:CommonTypes.Port) -> y.Id = id) x.OutputPorts) |> function |Some a -> [a] |None -> []) model.Symbol.Symbols
              |>List.head

    let wires = 
        model.Wires 
        |> List.map (fun w ->
            let start = (convertIdToPort 0 w.SrcPort).PortPos
            let final = (convertIdToPort 1 w.TargetPort).PortPos
            printfn "startport %A finalport %A startportid %A finalportid %A" start final w.SrcPort w.TargetPort
            let vertex = if model.AutoRouting
                          then newWireRoute final start model
                          else w.Vertices
            let BusWidth = w.BusWidth
            //let vertex = newWireRoute final start model
            let Selected = w.Selected
            let Highlighted = w.Highlighted 
            let wireColour = match (BusWidth, Highlighted, Selected) with 
                             | (_, _, true) -> "yellow"
                             | (_, true, false) -> "red"
                             | (1,false, false) -> "black"
                             | (_, false, false )-> "darkorchid"
            let props = {
                key = w.Id
                WireP = w
                Selected= w.Selected
                BusWidth = w.BusWidth
                SrcP = start 
                TgtP = final 
                Vertices = vertex
                //ColorP = model.Color.Text()
                ColorP = wireColour
                StrokeWidthP = "2px"
                Highlighted = w.Highlighted
                IsDragging = false 
                LastDragPos = vertex 
                // PortInUse = CommonTypes.Port.PortInUse
                }
            singleWireView props) // pass in the props for this given wire into singleWireView
    let symbols = Symbol.view model.Symbol (fun sMsg -> dispatch (Symbol sMsg)) 
    g [] [(g [] wires); symbols] // displaying the wires and symbols 

/// A function which creates a new Bounding Box for a wire. 
let createNewBB outp inp model= 
    wireBoundingBoxes (newWireRoute (inp) (outp) model)

/// A function which creates a new wire. This is called from the AddWire message in the update function. 
let createNewWire (sourcePort:string) (targetPort:string) (model:Model) : Wire =
    let convertIdToPort inOut (id:string) =
        match inOut with 
        |1 -> List.collect (fun (x:Symbol.Symbol) -> (List.tryFind (fun (y:CommonTypes.Port) -> y.Id = id) x.InputPorts) |> function |Some a -> [a] |None -> []) model.Symbol.Symbols
              |>List.head
        |0 -> List.collect (fun (x:Symbol.Symbol) -> (List.tryFind (fun (y:CommonTypes.Port) -> y.Id = id) x.OutputPorts) |> function |Some a -> [a] |None -> []) model.Symbol.Symbols
              |>List.head

    if (convertIdToPort 0 targetPort).BusWidth <> (convertIdToPort 1 sourcePort).BusWidth
    then 
        let wireId = CommonTypes.ConnectionId (HelpersOne.uuid())
        {
            SrcSymbol = (convertIdToPort 1 sourcePort).HostId
            TargetSymbol = (convertIdToPort 0 targetPort).HostId
            Id = wireId 
            SrcPort = (convertIdToPort 0 targetPort).Id
            TargetPort = (convertIdToPort 1 sourcePort).Id
            Vertices = newWireRoute (convertIdToPort 1 sourcePort).PortPos (convertIdToPort 0 targetPort).PortPos model//CHECK
            Selected = false
            BusWidth = 1                                                            //need to set this to something
            Highlighted = true                                                            
            IsDragging = false
            LastDragPos =  newWireRoute   (convertIdToPort 1 sourcePort).PortPos (convertIdToPort 0 targetPort).PortPos model
        }
    else 
    let wireId = CommonTypes.ConnectionId (HelpersOne.uuid())
    {
        SrcSymbol = (convertIdToPort 1 sourcePort).HostId
        TargetSymbol = (convertIdToPort 0 targetPort).HostId
        Id = wireId 
        SrcPort = (convertIdToPort 0 targetPort).Id
        TargetPort = (convertIdToPort 1 sourcePort).Id
        Vertices =  newWireRoute   (convertIdToPort 1 sourcePort).PortPos (convertIdToPort 0 targetPort).PortPos model             //newWireRoute  (convertIdToPort 0 targetPortId) (convertIdToPort 1 sourcePortId)
        Selected = false
        BusWidth = (convertIdToPort 0 targetPort).BusWidth
        IsDragging = false
        LastDragPos =  newWireRoute   (convertIdToPort 1 sourcePort).PortPos (convertIdToPort 0 targetPort).PortPos model
        Highlighted = false
    }

let isEven (segId: int) (wir: Wire): Option<bool> = 
    let noOfSeg = (List.length wir.Vertices)-1
    printfn "segno %A" noOfSeg
    if segId = 0 || segId = noOfSeg-1
    then None
    else if segId % 2 = 1 
         then Some false 
         else Some true 
    //if noOfSeg = 5
    //then 
    //    match segId with
    //    | 1 -> Some false
    //    | 2 -> Some true
    //    | 3 -> Some false
    //    | _ -> None
    //else 
    //    match segId with
    //    | 1 -> Some false
    //    | _ -> None

let evenChange (currPos: XYPos) (mPos: XYPos): XYPos =
    {currPos with Y = mPos.Y}
let oddChange (currPos: XYPos) (mPos: XYPos): XYPos =
    printfn "False List"
    {currPos with X = mPos.X}

let updateVertices (segId: int) (wir: Wire) (mPos: XYPos) : XYPos list = 
    
    let trueList segindex = 
        printfn "True List"
        wir.Vertices 
        |> List.indexed
        |> List.map (fun (index,vertex) -> if (index = segindex || index = segindex+1) 
                                           then evenChange vertex mPos //{X=400.;Y=400.} 
                                           else vertex)  

    let falseList idx = 
        wir.Vertices 
        |> List.indexed
        |> List.map (fun (i,v) -> 
                                  if (i = idx || i = idx+1) 
                                  then oddChange v mPos //{X=400.;Y=400.} 
                                  else v)
    
    
    match isEven segId wir with 
    | Some true -> trueList segId
    | Some false -> falseList segId
    | None -> failwithf "Error"





    /// Initialisation function. Begins with no wires, and uses the Symbol model as a base. 
let init () = 
    let symbols, cmd = Symbol.init()
    {Wires = []; Symbol = symbols;Color = CommonTypes.Red; wBB = []; AutoRouting=false }, Cmd.none  //; AutoRouting=true

/// check through the symbols - if IsSelected=true, filter the wires
    /// after, check through the wires - Filter(if Selected=true, remove)

let update (msg : Msg) (model : Model): Model*Cmd<Msg> =
    printfn "autoroute %A" model.AutoRouting
    let convertIdToPort inOut (id:string) =
        match inOut with 
        |1 -> List.collect (fun (x:Symbol.Symbol) -> (List.tryFind (fun (y:CommonTypes.Port) -> y.Id = id) x.InputPorts) |> function |Some a -> [a] |None -> []) model.Symbol.Symbols
              |>List.head
        |0 -> List.collect (fun (x:Symbol.Symbol) -> (List.tryFind (fun (y:CommonTypes.Port) -> y.Id = id) x.OutputPorts) |> function |Some a -> [a] |None -> []) model.Symbol.Symbols
              |>List.head
    match msg with


    | Symbol sMsg -> 
        //cmoe back to this - moving the symbol and its effect on wires
        // NOT 
        let sm,sCmd = Symbol.update sMsg model.Symbol 
        match sMsg with
        |Symbol.Dragging(_,_,_) -> {model with Symbol=sm; AutoRouting = true }, Cmd.map Symbol sCmd
        |_ -> if model.AutoRouting = true 
              then let newWire = List.map (fun (w:Wire) -> {w with Vertices = (newWireRoute  (convertIdToPort 1 w.TargetPort).PortPos (convertIdToPort 0 w.SrcPort).PortPos model)}) model.Wires
                   {model with Symbol=sm; AutoRouting = false; Wires= newWire }, Cmd.map Symbol sCmd
              else {model with Symbol=sm; AutoRouting = false }, Cmd.map Symbol sCmd
        

    | AddWire (inputPort,outputPort) -> 
        let addNewWire = (createNewWire inputPort outputPort model) :: model.Wires //NOT 
        printfn "add %A" addNewWire
        let addNewWireBB = (createNewBB  (convertIdToPort 0 outputPort).PortPos (convertIdToPort 1 inputPort).PortPos) model :: model.wBB //NOT
        printfn "add %A" addNewWireBB
        {model with Wires=addNewWire; wBB=addNewWireBB; AutoRouting = false}, Cmd.none

    | MouseMsg mMsg ->  model , Cmd.ofMsg (Symbol (Symbol.MouseMsg mMsg))

    | SnaptoGrid (symToSel, wireAndSegList ) -> model , Cmd.ofMsg (Symbol (Symbol.SnapSymbolToGrid symToSel))

    // |RunBusWidthInference -> runBusWidthInference model, Cmd.none
    
    | DeleteWire ->
        
        let checkWiress (wires,bBoxes)= 
            let checkWire (wiresList, bBoxesList) (wireTest:Wire) boundingBoxTest = if wireTest.Selected = true
                                                                                      then (wiresList@[wireTest], bBoxesList@[boundingBoxTest])
                                                                                      else (wiresList, bBoxesList)
            List.fold2 checkWire ([],[]) wires bBoxes
        let remainingWiresAndBoxes = 
            List.fold (fun (wires,bBoxes) (symbol:Symbol.Symbol) -> if symbol.IsSelected=true
                                                                    then let filter (filteredWires, filteredBBoxes) (wire:Wire) boundingBox =
                                                                                if wire.SrcSymbol = string symbol.Id || wire.TargetSymbol = string symbol.Id
                                                                                then (filteredWires, filteredBBoxes)
                                                                                else match wire.Selected with                                      //either check if everytime or put this in a separate traversal - should be here imo
                                                                                     | false ->  (filteredWires@[wire], filteredBBoxes@[boundingBox])
                                                                                     | true -> (filteredWires, filteredBBoxes)
                                                                         List.fold2 filter ([],[]) wires bBoxes
                                                                    else (wires,bBoxes) ) (model.Wires,model.wBB) model.Symbol.Symbols
        let remainingWires = fst (checkWiress remainingWiresAndBoxes) //(fst ) (snd remainingWiresAndBoxes))
        let remainingBbox = snd (checkWiress remainingWiresAndBoxes)//(fst remainingWiresAndBoxes) (snd remainingWiresAndBoxes))
        {model with Wires=remainingWires; wBB=remainingBbox}, Cmd.ofMsg (Symbol (Symbol.DeleteSymbol))

    | ToggleSelect (symToSel, wireAndSegList ) ->
        let wiresToSel,segmentsList = List.unzip wireAndSegList
        let selectWires = 
            List.map (fun (wire:Wire) -> if List.contains wire wiresToSel
                                         then {wire with Selected = not wire.Selected} 
                                         else wire ) model.Wires
        {model with Wires=selectWires}, Cmd.ofMsg (Symbol (Symbol.ToggleSymbol symToSel))

    // | Hovering (symToSel, wireAndSegList ) -> 
    //     let wiresToSel,segmentsList = List.unzip wireAndSegList
    //     let selectWires = 
    //         List.map (fun (wire:Wire) -> if List.contains wire wiresToSel
    //                                      then {wire with Highlighted = true} 
    //                                      else wire ) model.Wires
    //     {model with Wires=selectWires}, Cmd.ofMsg (Symbol (Symbol.Hovering symToSel))

    | Dragging ((symbolUpdated,wireAndSegList), prevPos, mousePos) ->
        //probably need to unselect the other selected wires?
        
        match wireAndSegList, symbolUpdated with
        | [], [symbolUpdated] -> let newBB = List.map (fun w -> wireBoundingBoxes (newWireRoute (convertIdToPort 1 w.TargetPort).PortPos (convertIdToPort 0 w.SrcPort).PortPos  model)) model.Wires
                                 {model with AutoRouting=true; wBB = newBB}, Cmd.ofMsg (Symbol (Symbol.Dragging ([symbolUpdated],mousePos,prevPos)))    //autoroute       // with 
        | [wireUpdated,segIndex],_ -> printfn "autoroutingd"
                                      let updatedWires = List.map (fun wire -> if wire.Id = wireUpdated.Id
                                                                               then {wire with Vertices=updateVertices segIndex wireUpdated mousePos}
                                                                               else wire ) model.Wires
                                      {model with Wires=updatedWires; AutoRouting=false}, Cmd.none   //;
        

    | UpdateBoundingBoxes (symbolUpdated,wireAndSegList) ->     //can only have one element here right?
        // let wireUpdated,segmentsList = List.unzip wireAndSegList
        // let updatedBBoxes = 
        //     let findIndex = 
        //         model.Wires
        //         |> List.indexed
        //         |> List.filter (fun (idx, wire) -> wire.Id = wireUpdated.[0].Id )
        //     let decodeIndex = match findIndex with 
        //                       | [(idx, wire)] -> idx
        //                       | _ -> failwithf "Error"

        //     model.wBB 
        //     |> List.indexed
        //     |> List.map (fun (index, bb) -> if index = decodeIndex then wireBoundingBoxes wireUpdated.[0].Vertices else bb )
        // 
        model, Cmd.ofMsg (Symbol (Symbol.UpdateBBoxes (symbolUpdated)))

        // let updatesBbox =
        //     let indexforBbox = List.findIndex (fun w -> w.Id = rank) model.Wires
        //     let updateBBox index boxList =
        //         let diff2 = posDiff pagePos model.Wires.[index].LastDragPos
        //         let {X = correctX; Y= correctY} =  posAdd (model.Wires.[index].Pos) diff2 
        //         if index = indexforBbox then [correctX-10.,correctY-10.;correctX+10.+model.Wires.[index].W, correctY-10.; correctX+10.+model.Wires.[index].W, correctY+10. + model.Wires.[index].H; correctX-10.,correctY+10.+ model.Wires.[index].H] else boxList
        //     List.mapi (fun i p -> updateBBox i p) model.wBB
        // {model with Wires = dSymbols; wBB = updatesBbox}, Cmd.none
    // | _ -> failwithf "Unmatched in BusWire Update function"

        |_ -> {model with AutoRouting = false}, Cmd.none
//---------------Other interface functions--------------------//

/// look up wire in WireModel
//let wire (wModel: Model) (wId: CommonTypes.ConnectionId): Wire =
//    wModel.Wires 
//    |> List.contains (fun wire -> wire.Id = wId)

///// Given a point on the canvas, returns the wire ID of a wire within a few pixels
///// or None if no such. Where there are two close wires the nearest is taken. Used
///// to determine which wire (if any) to select on a mouse click
//let wireToSelectOpt (wModel: Model) (pos: XYPos) : CommonTypes.ConnectionId option = 

//    let listOfBoundingBoxes = wModel.Wires |> List.map (fun x -> x.BoundingBoxes)
//    let isInside bblst  =
//        let inSeg lst =
//            let wireId, box1, box2 = lst
//            if (pos.X>= box1.X && pos.X <= box2.X) && (pos.Y >= box1.Y && pos.Y <= box2.Y) then (true, wireId) else (false, wireId)            
//        bblst 
//        |> List.map inSeg
    
//    let mapToBB = 
//        listOfBoundingBoxes 
//        |> List.collect isInside
//        |> List.filter (fun (x,y) -> x=true)

//    match mapToBB with 
//    | [(true, wireId)] -> Some wireId
//    | _ -> None
//----------------------interface to Issie-----------------------//
let extractWire (wModel: Model) (sId:CommonTypes.ComponentId) : CommonTypes.Component= 
    failwithf "Not implemented"
    
let extractWires (wModel: Model) : CommonTypes.Component list = 
    failwithf "Not implemented"

/// Update the symbol with matching componentId to comp, or add a new symbol based on comp.
let updateSymbolModelWithComponent (symModel: Model) (comp:CommonTypes.Component) =
    failwithf "Not Implemented"


    




    //first removing wires that are on symbols
        // let remainingWiresAndBoxes = 
        //     let checkWire (wiresList, bBoxesList) (wire:Wire) boundingBox =
        //         let areAttachedSymbolsSelected =
        //             match wire with
        //             | Symolwire.SrcPort.HostId
        //         match wire.Selected with
        //         | true -> (wiresList, bBoxesList)
        //         | false -> match areAttachedSymbolsSelected with
        //                    | true -> (wiresList, bBoxesList)
        //                    | false -> (wiresList@[wire], bBoxesList@[boundingBox])
        //     List.fold2 checkWire ([],[]) model.Wires model.wBB
        // let wiresConnectedToSymbols = List.map () model.Wires
        // //then removing remaining wires selected
        // let selectedList = 
        //     let checkWire (wiresList, bBoxesList) (wireTest:Wire) boundingBox= 
        //         if wireTest.Selected = true
        //         then (wiresList@[wireTest], bBoxesList@[boundingBox])
        //         else (wiresList, bBoxesList)
        //     List.fold2 checkWire ([],[]) model.Wires model.wBB