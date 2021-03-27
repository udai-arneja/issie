module Sheet

open Fulma
open Fable.React
open Fable.React.Props
open Browser
open Elmish
open Elmish.React
open HelpersOne

type Model = {
    Wire: BusWire.Model
    IsWiring: (Option<CommonTypesOne.Port> * Option<CommonTypesOne.Port>)   //Input/Output * (portId * portId)       //do we need null for the first one - so does it need to be an option
    IsSelecting: CommonTypesOne.ComponentId list * (BusWire.Wire * int) list        //Symbols * Wires
    IsDropping: bool
    IsDraggingList: int * XYPos
    MultiSelectBox: bool * XYPos * XYPos  //boxOrWire,startPos, endPos multi-select box
    Restore: BusWire.Model
    LastOp: HelpersOne.MouseOp;
    Zoom : float
    LastDragPos : XYPos
    }

type KeyboardMsg =
    | CtrlS | AltC | AltV | AltZ | AltShiftZ | DEL| Ctrl | AltUp |AltDown | AltO

type Msg =
    | Wire of BusWire.Msg
    | KeyPress of KeyboardMsg


type SelectingBox={
    TopCorner: XYPos
    BottomCorner: XYPos
}

//helper functions

//ISSIE FUNCTIONS





//

let zoom = 1.0

let dimensions startPos endPos = sprintf "%f,%f %f,%f %f,%f %f,%f" startPos.X startPos.Y startPos.X endPos.Y endPos.X endPos.Y endPos.X startPos.Y



//display

let displaySvgWithZoom (zoom:float) (svgReact: ReactElement) (dispatch: Dispatch<Msg>) (model:Model)=
    let sizeInPixels = sprintf "%.2fpx" ((1000. * 3.))
    /// Is the mouse button currently down?
    let mDown (ev:Types.MouseEvent) = 
        if ev.buttons <> 0. then true else false
    /// Dispatch a BusWire MouseMsg message
    /// the screen mouse coordinates are compensated for the zoom transform
    let mouseOp op (ev:Types.MouseEvent) = 
        dispatch <| Wire (BusWire.MouseMsg {Op = op ; Pos = { X = ev.clientX / model.Zoom ; Y = ev.clientY / model.Zoom}})
    let (boxOrWire, startPos, endPos) = model.MultiSelectBox
    let backgroundSize = sprintf "%fpx %fpx" (30.* model.Zoom) (30.*model.Zoom)
    let background = "linear-gradient(to right, LightGrey 2px, transparent 1px), linear-gradient(to bottom, LightGrey 2px, transparent 1px)"
    div [ Style 
            [ 
                Height "100vh" 
                MaxWidth "100vw"
                CSSProp.OverflowX OverflowOptions.Auto 
                CSSProp.OverflowY OverflowOptions.Auto
                // BackgroundSize backgroundSize
                // BackgroundImage background
            ] 
          OnMouseDown (fun ev -> (mouseOp Down ev))
          OnMouseUp (fun ev -> (mouseOp Up ev))
          OnMouseMove (fun ev -> mouseOp (if mDown ev then Drag else Move) ev)
        ]
        [ svg
            [ Style 
                [
                  
                    match model.IsWiring with 
                    |(None, Some Port) -> Cursor "grabbing"
                    |(Some Port, None) -> Cursor "grabbing"
                    |(None, None) -> Cursor "default"

                    Border "3px solid green"
                    Height "100vh"
                    Width "100vw"  
                    BackgroundSize backgroundSize
                    BackgroundImage background        
                ]
            ]
            [ g // group list of elements with list of attributes
                [ Style [Transform (sprintf "scale(%f)" model.Zoom)]] // top-level transform style attribute for zoom
                [
                match boxOrWire with 
                | true -> polygon [
                                    SVGAttr.Points (dimensions startPos endPos)
                                    SVGAttr.StrokeWidth "1px"
                                    SVGAttr.Stroke "Black"
                                    SVGAttr.FillOpacity 0.1
                                    SVGAttr.Fill "Blue"
                                ][]
                          svgReact
                     // the application code
                | _ -> svgReact
                ]
            ]
        ]

let view (model:Model) (dispatch : Msg -> unit) =
    let wDispatch wMsg = dispatch (Wire wMsg)
    let wireSvg = BusWire.view model.Wire wDispatch
    displaySvgWithZoom zoom wireSvg dispatch model

let inSelBox (model:Model) (sc:XYPos) (ec:XYPos): (CommonTypesOne.ComponentId list * (BusWire.Wire*int) list) =
    let corners = if sc.X < ec.X     //dragging left to right
                      then if sc.Y < ec.Y
                           then {TopCorner=sc;BottomCorner=ec}          //dragging up to down
                           else {TopCorner={X=sc.X;Y=ec.Y};BottomCorner={X=ec.X;Y=sc.Y}}    //dragging down to up
                      else if sc.Y > ec.Y    //dragging right to left
                          then {TopCorner=ec;BottomCorner=sc}  //dragging down to up
                          else {TopCorner={X=ec.X;Y=sc.Y};BottomCorner={X=sc.X;Y=ec.Y}}   //dragging up to down
    let overlap index (pos1,pos2) = if corners.TopCorner.X<pos1.X && corners.BottomCorner.X>pos1.X 
                                        ||corners.TopCorner.X<pos2.X && corners.BottomCorner.X>pos2.X
                                    then if corners.TopCorner.Y<pos1.Y && corners.BottomCorner.Y>pos1.Y
                                              ||corners.TopCorner.Y<pos2.Y && corners.BottomCorner.Y>pos2.Y
                                           then Some index         //use index to get the symbol id
                                           else None
                                    else None
    let symbolscontained =
        List.mapi overlap model.Wire.Symbol.SymBBoxes
        |> List.map (fun indexOption -> match indexOption with
                                        | Some index -> Some (model.Wire.Symbol.Symbols.[index].Id)
                                        | None ->  None)
        |> List.choose (fun x-> x )
    // FINDING WIRES IN THE MULTI-SELECT BOX - NOT CURRENTLY NEEDED
    // let wirescontained = 
    //     List.mapi (fun index segmentsList -> 
    //                         List.tryPick (overlap index) segmentsList
    //                         ) model.Wire.wBB
    //     |> List.map (fun val1 -> match val1 with
    //                              | Some index -> Some (model.Wire.Wires.[index].Id)
    //                              | None ->  None)
    //     |> List.choose (fun x->x)
    (symbolscontained,[])


let wireToSelectOpt (wModel: BusWire.Model) (pos: XYPos) : (BusWire.Wire * int) list = //checks if point is in wire bounding box
    let isInside bBoxList wire= //gives you the wire bb list 
        let inSeg indexSeg segment = //list of bounding boxes 
            let (box1, box2) = match segment,(indexSeg%2) with 
                               |(a,b),1 -> (b,a)
                               |(a,b),0 -> (a,b)
                               | _ -> failwithf "Not Implemented"
            if ((pos.X <= box1.X && pos.X >= box2.X) && (pos.Y <= box2.Y && pos.Y >= box1.Y)) ||  ((pos.X >= box1.X && pos.X <= box2.X) && (pos.Y >= box2.Y && pos.Y <= box1.Y))
            then (true, wire, indexSeg) 
            else (false, wire, indexSeg)            
        bBoxList 
        |> List.mapi (fun indexSeg y -> inSeg indexSeg y)
    
    let vertices = List.map (fun (i:BusWire.Wire) -> i.Vertices) wModel.Wires

    let mapToBB = 
        wModel.wBB 
        |> List.mapi (fun index wireBBoxes -> isInside wireBBoxes wModel.Wires.[index])
        |> List.collect id
        |> List.filter (fun (x,y,indexSeg) -> x=true) 

    match mapToBB with 
    | [(true, wire,indexSeg)] ->printfn "wire %A seg %A"  wire indexSeg
                                [(wire,indexSeg)]
    | _ -> []

let update (msg : Msg) (model : Model): Model*Cmd<Msg> =

    match msg with
    | Wire (BusWire.MouseMsg {Op = mouseState ; Pos = { X = mX; Y = mY}}) ->

        //helper functions
        let mousePos = {X=mX;Y=mY}

        let boundingBoxSearchS = match List.tryFindIndex (fun (co1,co2) -> co1.X<mX && co2.X>mX && co1.Y<mY && co2.Y>mY) model.Wire.Symbol.SymBBoxes with
                                 | Some index -> [model.Wire.Symbol.Symbols.[index]]
                                 | None -> []

        let boundingBoxSearchW = wireToSelectOpt model.Wire mousePos

        let boundingBoxSearchP (symbol: Symbol.Symbol): CommonTypesOne.Port list=
            let dist (pos1:XYPos) (pos2:XYPos) = sqrt((pos1.X-pos2.X)**2. + (pos1.Y-pos2.Y)**2.)
            let portCalculator portlist =
                    printfn "over a port"
                    match List.tryFind (fun (port:CommonTypesOne.Port) -> (dist port.PortPos mousePos)<5.) portlist with
                    | Some port -> [port]
                    | None -> []
            if mousePos.X <= (symbol.Pos.X+(symbol.W/2.))
            then portCalculator symbol.InputPorts
            else portCalculator symbol.OutputPorts

        let addWire (ports: string * string): Msg = (Wire <| BusWire.AddWire ports)

        match mouseState with
        
        | Down -> 
                //   if model.IsDropping = true 
                //   then {model with IsDropping=false;LastOp=Down}, Cmd.none
                //   else 
                  match boundingBoxSearchS with
                       |[sym] -> match boundingBoxSearchP sym with
                                 | [port] -> match model.IsWiring with
                                              | (None, None) -> match port.PortType with
                                                                | CommonTypesOne.Input -> {model with IsWiring=(Some port, None);LastOp=Down;LastDragPos=mousePos},Cmd.ofMsg (Wire <| BusWire.Symbol (Symbol.ShowValidPorts (CommonTypesOne.ShowOutputsOnly, port.Id, mousePos)) )
                                                                | CommonTypesOne.Output -> {model with IsWiring=(None, Some port);LastOp=Down;LastDragPos=mousePos},Cmd.ofMsg (Wire <| BusWire.Symbol (Symbol.ShowValidPorts (CommonTypesOne.ShowInputsOnly, port.Id, mousePos)) )
                                              | (None, Some outputPort)-> match port.PortType with
                                                                          | CommonTypesOne.Input -> {model with IsWiring=(None,None);LastOp=Down;LastDragPos=mousePos}, Cmd.ofMsg (addWire (string port.Id,string outputPort.Id))
                                                                          | CommonTypesOne.Output -> {model with IsWiring=(None,None);LastOp=Down;LastDragPos=mousePos},Cmd.none
                                              | (Some inputPort, None) -> match port.PortType with
                                                                          | CommonTypesOne.Output -> {model with IsWiring=(None,None);LastOp=Down;LastDragPos=mousePos}, Cmd.ofMsg (addWire (string inputPort.Id,string port.Id))
                                                                          | CommonTypesOne.Input -> {model with IsWiring=(None,None);LastOp=Down;LastDragPos=mousePos},Cmd.none
                                               | _ -> failwithf "Not implemented - Down Sheet Update function ~ 219"          
                                 | _ -> {model with IsSelecting = ([sym.Id],[]); LastOp=Down;LastDragPos=mousePos}, Cmd.none
                                 
                       | _ -> match boundingBoxSearchW with 
                               |[wireAndSeg] -> {model with IsSelecting = ([],[wireAndSeg]); LastOp=Down;LastDragPos=mousePos}, Cmd.none         //reset wiring to none
                               |_ -> {model with IsSelecting = ([],[]);LastOp=Down;MultiSelectBox=(true,mousePos,mousePos);LastDragPos=mousePos}, Cmd.ofMsg (Wire <| BusWire.ToggleSelect ([],[]))

        | Up -> match model.LastOp with
                | Drag -> match model.MultiSelectBox with
                          |(true,p1,p2) -> {model with MultiSelectBox=(false,{X=0.;Y=0.},{X=0.;Y=0.});LastOp=Up;LastDragPos=mousePos}, Cmd.ofMsg (Wire <| BusWire.ToggleSelect (inSelBox model p1 p2))// (symbInSelBox model p1 p2 , wireInSelBox model.Wire p1 p2) )//check if in bounding boxes
                          | _ -> {model with LastOp=Up;LastDragPos=mousePos}, Cmd.ofMsg (Wire <| BusWire.SnaptoGrid model.IsSelecting) //Cmd.ofMsg (Wire <| BusWire.UpdateBoundingBoxes model.IsSelecting) //   Cmd.ofMsg (updateBBoxes model.IsSelecting) //interface required

                | Down -> {model with IsSelecting = ([],[]);LastOp=Up;LastDragPos=mousePos;MultiSelectBox=(false,{X=0.;Y=0.},{X=0.;Y=0.})}, Cmd.ofMsg (Wire <| BusWire.ToggleSelect model.IsSelecting)
                | _ -> {model with LastOp=Up;LastDragPos=mousePos}, Cmd.none

        | Drag -> match model.MultiSelectBox with 
                  |(true, p1, p2) -> {model with IsSelecting=([],[]);LastOp=Drag;MultiSelectBox=(true,p1,mousePos);LastDragPos=mousePos}, Cmd.none
                  | _ -> {model with LastOp=Drag;LastDragPos=mousePos}, Cmd.ofMsg (Wire <| BusWire.Dragging (model.IsSelecting, model.LastDragPos, mousePos))//BusWire.Symbol (Symbol.Dragging ((fst model.IsSelecting),mousePos, prevPos))) //send to symbol to move symbols lol
                  //   |(false, p1, prevPos) -> match model.LastOp with 
                //                            |Down -> {model with LastOp=Drag; MultiSelectBox=(false, {X=0.;Y=0.}, mousePos);LastDragPos=mousePos}, Cmd.ofMsg (Wire <| BusWire.Dragging (model.IsSelecting, model.LastDragPos, mousePos) ) 
                //                            |Drag -> {model with LastOp=Drag; MultiSelectBox=(false, {X=0.;Y=0.}, mousePos);LastDragPos=mousePos}, Cmd.ofMsg (Wire <| BusWire.Dragging (model.IsSelecting, model.LastDragPos, mousePos))//BusWire.Symbol (Symbol.Dragging ((fst model.IsSelecting),mousePos, prevPos))) //send to symbol to move symbols lol
                //                         //    | _ -> model, Cmd.none
        | Move -> match model.IsWiring with 
                  |(None,None) -> match boundingBoxSearchS with
                                  | [symbol] -> {model with LastOp=Move;LastDragPos=mousePos}, Cmd.ofMsg (Wire <| BusWire.Symbol (Symbol.Hovering [symbol.Id]))
                                  | _ -> {model with LastOp = Move;LastDragPos=mousePos}, Cmd.none
                  |(None,Some port) -> {model with LastOp=Move;LastDragPos=mousePos}, Cmd.ofMsg (Wire <| BusWire.Symbol (Symbol.ShowValidPorts (CommonTypesOne.ShowInputsOnly, port.Id, mousePos)) )
                  |(Some port,None) -> {model with LastOp=Move;LastDragPos=mousePos}, Cmd.ofMsg (Wire <| BusWire.Symbol (Symbol.ShowValidPorts (CommonTypesOne.ShowOutputsOnly, port.Id, mousePos)) )
                  | _ -> failwithf "Not implemented - Move Sheet Update function ~ 253" 

    |Wire wMsg -> 
        let wModel, wCmd = BusWire.update wMsg model.Wire //send message
        {model with Wire = wModel}, Cmd.map Wire wCmd

    | KeyPress AltShiftZ -> 
        printStats() // print and reset the performance statistics in dev tools window
        model, Cmd.none

    | KeyPress CtrlS -> // add symbol and create a restore point
        let wModel, wCmd = BusWire.update (BusWire.Msg.Symbol (Symbol.AddSymbol ([2;2], [2], CommonTypesOne.Nor))) model.Wire    // [1], [1] - this needs to be different for different types        Custom {Name="Kurt";InputLabels=[("Udai",1);("Simi",1);("Gabs",1)];OutputLabels=[("Karl",1)]})
        {model with Wire = wModel; IsDropping = true; Restore = model.Wire}, Cmd.map Wire wCmd
    
    |KeyPress DEL ->
        {model with IsSelecting=([],[])}, Cmd.ofMsg (Wire <| BusWire.DeleteWire)

    | KeyPress AltZ -> {model with Wire = model.Restore; IsSelecting=([],[]);IsDraggingList=(0, {X=0.;Y=0.}); IsDropping=false; IsWiring=(None,None); Restore=model.Wire}, Cmd.none //undo and reset everything
                // IsDragSelecting = (0, {X=0.;Y=0.}, {X=0.;Y=0.});
    // | KeyPress AltUp ->
    //     printfn "Zoom In"
    //     {model with Zoom=model.Zoom+0.1}, Cmd.none

    | KeyPress AltDown ->
        printfn "Zoom Out"
        {model with Zoom=model.Zoom-0.1}, Cmd.none

    // | KeyPress AltUp -> model, Cmd.ofMsg (Wire <| BusWire.RunBusWidthInference )
    
    // | KeyPress PrintSelected ->
    //     let nothing = 
    //         List.map (fun symbol -> printfn symbol.IsSelected;symbol) model.Wire.Symbol.Symbols
    //     {model with Wire with Symbol with Symbols=nothing} , Cmd.none

    | KeyPress s -> 
        let c =
            match s with
            | AltC -> CommonTypesOne.Blue
            | AltV -> CommonTypesOne.Green
            | _ -> CommonTypesOne.Grey
        model, Cmd.none


let init() = 
    let model,cmds = (BusWire.init)() //initial model state
    {
        Wire = model
        IsWiring = (None, None)
        IsSelecting= ([], [])
        IsDropping= false
        IsDraggingList = (0, {X=0.;Y=0.})
        MultiSelectBox = (false, {X=0.;Y=0.}, {X=0.;Y=0.})
        Restore = model 
        LastOp = Move
        Zoom = 1.0
        LastDragPos={X=0.;Y=0.}
    }, Cmd.map Wire cmds





    

// let wireInSelBox (wModel:BusWire.Model) startPos finalPos =  //checks if wire bounding box within box 
//     let innerLayer start fn bblst = 
//         let innerSeg lst = 
//             let (box1, box2) = lst
//             {X = fn box1.X box2.X; Y = (fn box1.Y box2.Y)}            
//         bblst 
//         |> List.fold (fun acc y -> {X = (fn (innerSeg y).X acc.X); Y = (fn (innerSeg y).Y acc.Y)}) start
//     let maxCoord = 
//         wModel.wBB 
//         |> List.map (innerLayer {X=0.;Y=0.} max)
//     let minCoord =
//         wModel.wBB 
//         |> List.map (innerLayer {X=1000.;Y=1000.} min)
//     List.filter (fun a -> maxCoord.[a].X <= finalPos.X &&  maxCoord.[a].Y <= finalPos.Y) [0..(wModel.wBB.Length-1)]
//     |> List.filter (fun b -> minCoord.[b].X >= startPos.X && minCoord.[b].Y >= startPos.Y)
//     |> List.map (fun c -> wModel.Wires.[c].Id)

// let symbInSelBox (model:Model) (sc:XYPos) (ec:XYPos): (CommonTypesOne.ComponentId) list=     //sc : start corner, ec: end corner
//     let corners = if sc.X < ec.X     //dragging left to right
//                       then if sc.Y < ec.Y
//                            then {TopCorner=sc;BottomCorner=ec}          //dragging up to down
//                            else {TopCorner={X=sc.X;Y=ec.Y};BottomCorner={X=ec.X;Y=sc.Y}}    //dragging down to up
//                       else if sc.Y > ec.Y    //dragging right to left
//                           then {TopCorner=ec;BottomCorner=sc}  //dragging down to up
//                           else {TopCorner={X=ec.X;Y=sc.Y};BottomCorner={X=sc.X;Y=ec.Y}}   //dragging up to down

//     let overlap index (pos1,pos2) = if corners.TopCorner.X<pos1.X && corners.BottomCorner.X>pos1.X 
//                                         ||corners.TopCorner.X<pos2.X && corners.BottomCorner.X>pos2.X
//                                     then if corners.TopCorner.Y<pos1.Y && corners.BottomCorner.Y>pos1.Y
//                                               ||corners.TopCorner.Y<pos2.Y && corners.BottomCorner.Y>pos2.Y
//                                            then Some (model.Wire.Symbol.Symbols.[index].Id)          //use index to get the symbol id
//                                            else None
//                                     else None

//     List.mapi overlap model.Wire.Symbol.SymBBoxes
//     |> List.choose (fun x->x)

        // let sModel, sCmd = BusWire.update (BusWire.Symbol (Symbol.DeleteSymbol)) model.Wire
        // let newWSelectList = 
        //     if symList <> [] //if symbols have been selected then check whether wires connected also have to be deleted
        //     then
        //         let srcPorts = List.map (fun (x:BusWire.Wire) -> x.SrcPort) model.Wire.Wires //take all the inputports ids from wires
        //         let tgtPorts = List.map (fun (x:BusWire.Wire) -> x.TargetPort) model.Wire.Wires //take all the outputports ids from wires
        //         let symbolsList = 
        //             List.map (fun x -> List.item x model.Wire.Symbol.Symbols) symList //collect all the selected symbols
        //         let selectedSymbolInputs = //check if portids in the wires and selected symbols match for input ports -> if so then take the index of the wire 
        //             symbolsList
        //             |> List.map (fun (x:Symbol.Symbol) -> List.collect (fun (y:CommonTypesOne.Port) -> [y.Id]) x.InputPorts)
        //             |> List.collect (fun lst -> List.collect (fun y -> List.tryFindIndex (fun s -> s = y) srcPorts |> function |Some a -> [a] |None -> []) lst) 
        //         let selectedSymbolOutputs= //check if portids in the wires and selected symbols match for output ports -> if so then take the index of the wire 
        //             symbolsList
        //             |> List.map (fun (x:Symbol.Symbol) -> List.collect (fun (y:CommonTypesOne.Port) -> [y.Id]) x.OutputPorts) 
        //             |> List.collect (fun lst -> List.collect (fun y -> List.tryFindIndex (fun s -> s = y) tgtPorts |> function |Some a -> [a] |None -> []) lst) 
        //         let inPlusOut = List.fold (fun acc w -> selectAlready 0 acc w) selectedSymbolInputs selectedSymbolOutputs //check whether the wires have already been selected 
        //         List.fold (fun acc w -> selectAlready 0 acc w) inPlusOut wireList
        //     else 
        //         wireList
        // let wModel, wCmd = BusWire.update (BusWire.DeleteWire newWSelectList) sModel
        // {model with Wire = wModel; IsSelecting = ([],[]); Restore = model.Wire}, Cmd.map Wire wCmd //update model and reset selecting 
        // model, Cmd.none
        //need to 