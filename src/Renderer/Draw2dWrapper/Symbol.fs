module Symbol
open Fable.React
open Fable.React.Props
open Browser
open Elmish
open Elmish.React
open HelpersOne
open CommonTypesOne


//------------------------------------------------------------------------//
//-------------------------------Symbol Types-----------------------------//
//------------------------------------------------------------------------//

/// Model to generate one symbol (skeleton). Id is a unique Id
/// for the symbol shared with Issie Component type.
/// The real type will obviously be much larger.
/// Complex information that never changes (other than Id) should
/// probably not be here, but looked up via some function
/// from a more compact form, so that comparison of two Symbols to
/// determine are they the same is fast.


type Symbol =
    {
        LastDragPos : XYPos
        IsDragging : bool
        Id : CommonTypesOne.ComponentId
        Type : CommonTypesOne.ComponentType
        InputPorts : CommonTypesOne.Port list
        OutputPorts : CommonTypesOne.Port list
        Pos: XYPos
        H : float
        W : float
        IsSelected: bool
        PortStatus: PortVisibility
        IsSliding: PortVisibility * string * int * XYPos 
        // BoundingBox : 
    }

type Model = {
    Symbols: Symbol list
    SymBBoxes: (XYPos*XYPos)  List
    SingleOrMultiple: bool          //true - single
    }


//----------------------------Message Type-----------------------------------//

/// Messages to update symbol model
/// These are OK for the demo - but possibly not the correct messages for
/// a production system, where we need to drag groups of symbols as well,
/// and also select and deselect symbols, and specify real symbols, not circles
type Msg =
    /// Mouse info with coords adjusted form top-level zoom
    | MouseMsg of MouseT
    //| Dragging of sId : CommonTypesOne.ComponentId * pagePos: XYPos
    | Dragging of sId : CommonTypesOne.ComponentId list * pagePos: XYPos * prevPagePos: XYPos
    //| DraggingList of sId : CommonTypesOne.ComponentId list  * pagePos: XYPos * prevPagePos: XYPos
    //| EndDragging of sId : CommonTypesOne.ComponentId
    //| EndDraggingList of sId : CommonTypesOne.ComponentId list *pagePos:XYPos
    | AddSymbol of inputs: int list * outputs: int list * comp: CommonTypesOne.ComponentType
    | DeleteSymbol
    | UpdateSymbolModelWithComponent of CommonTypesOne.Component // Issie interface
    | ToggleSymbol of selectedSymbol:CommonTypesOne.ComponentId list // usually one thing
    | Hovering of portSymbol:CommonTypesOne.ComponentId list
    | ShowValidPorts of inOut:PortVisibility  * portId:string * mousePos:XYPos
    | UpdateBBoxes of CommonTypesOne.ComponentId list
    | SnapSymbolToGrid of CommonTypesOne.ComponentId list
    // | SelectSymbol of Symbol list

//---------------------------------helper types and functions----------------//



let posDiff (a:XYPos) (b:XYPos) =
    {X=a.X-b.X; Y=a.Y-b.Y}

let posAdd (a:XYPos) (b:XYPos) =
    {X=a.X+b.X; Y=a.Y+b.Y}

let posOf x y = {X=x;Y=y}

let gateWidth = 60.

let gateHeight = 80.

let circleRadius = 5.

let inOutLines = 10.

let rectum xPos yPos width height colour props  = // cheeky bit of kareem abstraction
        rect
            [
                X xPos
                Y yPos
                SVGAttr.Width width
                SVGAttr.Height height
                SVGAttr.Fill colour
                SVGAttr.FillOpacity 0.4
                SVGAttr.Stroke "Black"
                SVGAttr.StrokeWidth 2
            ]
            [ ]


let circus xPos yPos rad = // cheeky bit of kareem abstraction
    circle
        [
            SVGAttr.Cx xPos
            SVGAttr.Cy yPos
            SVGAttr.R rad
            SVGAttr.Fill "Transparent"
            SVGAttr.Stroke "Black"
            SVGAttr.StrokeWidth 2
        ] [ ]

let homotextual xPos yPos textAnchor domBaseline fontSize displayText = // cheeky bit of kareem abstraction
    text
        [ X xPos
          Y yPos
          Style
                [
                    TextAnchor textAnchor
                    DominantBaseline domBaseline
                    FontSize fontSize
                    FontWeight "Bold"
                    Fill "Black"
                ]
        ]
        [ str <| sprintf displayText ]

let creditLines x1Pos y1Pos x2Pos y2Pos width = // cheeky bit of kareem abstraction
    line
        [   X1 x1Pos
            Y1 y1Pos
            X2 x2Pos
            Y2 y2Pos
            Style
                [
                    CSSProp.Stroke "Black"
                    CSSProp.StrokeWidth width

                ]
        ]
        []

//-----------------------------Skeleton Model Type for symbols----------------//


let createPortList (comp:Symbol)(portType:CommonTypesOne.PortType)(portNumber:int)(width:int)(numPorts): CommonTypesOne.Port =
    let portPos=
        match comp.Type with
        // |RAM -> if portType = CommonTypesOne.Input
        //sthen {X=comp.Pos.X-10.;Y=(comp.Pos.Y+ (float(portNumber + 1))*(comp.H/6.))}
        //            else {X=(comp.Pos.X+comp.W);Y=(comp.Pos.Y+(float (portNumber + 1))*(comp.H/2.))}
        // |NbitAdder -> if portType = CommonTypesOne.Input
        //               then {X=comp.Pos.X-10.;Y=(comp.Pos.Y+ (float (portNumber)) + 1.)*(comp.H/4.)}
        //               else {X=(comp.Pos.X+comp.W);Y=(comp.Pos.Y+(float (portNumber + 1))*(comp.H/3.))}
        |_ -> match (portType, numPorts) with
              | (CommonTypesOne.Input, 1) -> {X=comp.Pos.X ;Y=(comp.Pos.Y+ (float (portNumber + 1))*(comp.H/2.))}
              | (CommonTypesOne.Input, 2) -> {X=comp.Pos.X ; Y=(comp.Pos.Y + (((float (portNumber))* comp.H)/2.) + comp.H/4.)}
              | (CommonTypesOne.Input, 3) -> {X=comp.Pos.X ;Y=(comp.Pos.Y+ 60.)} //(float (portNumber)) + 1.)*(comp.H/4.)
              | (_, 1) -> {X=(comp.Pos.X+comp.W);Y=(comp.Pos.Y + ( comp.H/2.) )}
              | (_, 2) -> {X=(comp.Pos.X+comp.W);Y=(comp.Pos.Y + ((float (portNumber))*2. + 1.)*(comp.H/4.))}  // this one
              |_ -> failwithf "Error on portlist"
    {
        CommonTypesOne.Port.Id = HelpersOne.uuid()
        CommonTypesOne.Port.PortNumber = Some portNumber
        CommonTypesOne.Port.PortType = portType
        CommonTypesOne.Port.HostId = string(comp.Id)
        CommonTypesOne.Port.PortPos = portPos
        CommonTypesOne.Port.BusWidth = width
        // CommonTypesOne.Port.PortInUse = false
    }

//-----------------------Skeleton Message type for symbols---------------------//

/// Symbol creation: a unique Id is given to the symbol, found from uuid.
/// The parameters of this function must be enough to specify the symbol completely
/// in its initial form. This is called by the AddSymbol message and need not be exposed.
let createNewSymbol (inputs: int list) (outputs: int list) (comp:CommonTypesOne.ComponentType) = //could match comp for symbols of different heights and widths
    let mainSymbol = {
                LastDragPos = {X=10.;Y=10.}
                IsDragging = false
                Id = CommonTypesOne.ComponentId (HelpersOne.uuid())
                Type = comp
                InputPorts = []
                OutputPorts = []
                Pos = {X=10.;Y=10.}
                H = 80.
                W = 60.
                IsSelected = false
                PortStatus = Invisible
                IsSliding = (ShowInputsOnly, "null", 0, {X=0.; Y=0.})
              }

    let InputPortsList = List.mapi (fun index width -> createPortList mainSymbol CommonTypesOne.PortType.Input index width (List.length inputs)) inputs
    let OutputPortsList = List.mapi (fun index width -> createPortList mainSymbol CommonTypesOne.PortType.Output index width (List.length outputs)) outputs
    {mainSymbol with InputPorts=InputPortsList; OutputPorts=OutputPortsList}

let createCustomSymbol (comp:CommonTypesOne.CustomComponentType) = 
    let mainSymbol = {
                LastDragPos = {X=10.;Y=10.}
                IsDragging = false
                Id = CommonTypesOne.ComponentId (HelpersOne.uuid())
                Type = Custom comp
                InputPorts = []
                OutputPorts = []
                Pos = {X=10.;Y=10.}
                H = max (80.) (float (max (List.length comp.InputLabels) (List.length comp.OutputLabels))*40.)
                W = 60.
                IsSelected = false
                PortStatus = Invisible
                IsSliding = (ShowInputsOnly, "null", 0, {X=0.; Y=0.})
              }  
    let _, inputBusWidths = List.unzip comp.InputLabels
    let _, outputBusWidths = List.unzip comp.OutputLabels
    let InputPortsList = List.mapi (fun index width -> createPortList mainSymbol CommonTypesOne.PortType.Input index width (List.length comp.InputLabels )) inputBusWidths //comp.InputLabels?
    let OutputPortsList = List.mapi (fun index width -> createPortList mainSymbol CommonTypesOne.PortType.Output index width (List.length comp.OutputLabels)) outputBusWidths
    {mainSymbol with InputPorts=InputPortsList; OutputPorts=OutputPortsList}            


let createNewBoundingBox (inputs: int list) (outputs: int list) (sym: Symbol)=
    ({X=0.;Y=0.},{X=sym.W+20.;Y=sym.H+20.})

    // +float(max (List.length inputs) (List.length outputs))*40.;Y=75.+float (max (List.length inputs) (List.length outputs))*40.})
    // [start.X-10., start.Y-10.; 110., start.Y-10.; 110., 75.+float (max inputno outputno)*40.; 75.+float (max inputno outputno)*40., 75.+float (max inputno outputno)*40.]

let portmove portId inputYes model =
    let findPort i (acc: CommonTypesOne.Port list) (x:Symbol)  =  match i with
                                                               |1 -> List.append (List.tryFind (fun (y:CommonTypesOne.Port) -> string y.Id = portId ) x.InputPorts |> function |Some a -> [a] |None -> []) acc
                                                               |0 -> List.append (List.tryFind (fun (y:CommonTypesOne.Port) -> string y.Id = portId ) x.OutputPorts |> function |Some a -> [a] |None -> []) acc
                                                               | _ -> failwithf "not implemented - findPort Function, Symbol line 152"
    let portReturn = match inputYes with
                     | ShowOutputsOnly -> List.fold (findPort 1) [] model |> List.head // potentially global for symbol
                     | ShowInputsOnly -> List.fold (findPort 0) [] model |> List.head
                     | _ -> failwithf "not implemented - portReturn Function, Symbol line 155"
    let symbolReturn = List.find (fun x -> x.Id = CommonTypesOne.ComponentId portReturn.HostId) model
    let portNumber = match portReturn.PortNumber with
                     |Some a -> a
                     | _ -> failwithf "not implemented - portNumber Function, Symbol line 159"
    (symbolReturn, portReturn, portNumber)

let init() =
    {Symbols=[]; SymBBoxes =[]; SingleOrMultiple=false}, Cmd.none

/// update function which displays symbols
let update (msg : Msg) (model : Model): Model*Cmd<'a>  =
    match msg with
    | AddSymbol(inputno, outputno, compType) ->
        //need to have anther sheet input parameter for when custom - this could be an option
        // let customInformation: CustomComponentType= 
        //     {Name="Kurtangle";InputLabels=[("Udai",1);("Simi",1)];OutputLabels=[("Karl",1)]}
        match compType with
        | CommonTypesOne.Custom customInformation -> let newSymbol = createCustomSymbol customInformation
                                                     let newBoundingBox = createNewBoundingBox inputno outputno newSymbol
                                                     let newSymbolList = List.rev (newSymbol::model.Symbols)
                                                     let newSymbolsBoundingBoxes = List.rev (newBoundingBox::model.SymBBoxes)
                                                     {model with Symbols=newSymbolList; SymBBoxes=newSymbolsBoundingBoxes} , Cmd.none
        | _ -> let newSymbol = createNewSymbol inputno outputno compType
               let newBoundingBox = createNewBoundingBox inputno outputno newSymbol
               let newSymbolList = List.rev (newSymbol::model.Symbols)
               let newSymbolsBoundingBoxes = List.rev (newBoundingBox::model.SymBBoxes)
               {model with Symbols=newSymbolList; SymBBoxes=newSymbolsBoundingBoxes} , Cmd.none
        
    | SnapSymbolToGrid (sId) ->

        let isSingleSelected = printfn "%A" model.Symbols
                               List.exists (fun sym -> sId=[sym.Id] && sym.IsSelected = false) model.Symbols

        let snapDifference coord= if coord % 30. < 15.
                                  then coord % 30.
                                  else -(30. - (coord % 30.))

        let roundCoord coord : float= coord - snapDifference coord

        let roundSymbolPos (symPos:XYPos) = {X=(roundCoord symPos.X);Y=(roundCoord symPos.Y)}

        let updatePortPos (currentPortPos:XYPos) (oldSymbolPos:XYPos) = {X=currentPortPos.X - (snapDifference oldSymbolPos.X);Y=currentPortPos.Y - (snapDifference oldSymbolPos.Y)}

        let snappedSymbols=
            match isSingleSelected with
            |true ->List.map (fun sym -> if [sym.Id] = sId
                                         then { sym with 
                                                    InputPorts = List.map (fun port -> {port with PortPos=updatePortPos port.PortPos sym.Pos}) sym.InputPorts
                                                    OutputPorts = List.map (fun port -> {port with PortPos=updatePortPos port.PortPos sym.Pos}) sym.OutputPorts
                                                    Pos = roundSymbolPos sym.Pos
                                                    LastDragPos = sym.Pos
                                                }
                                         else {sym with IsSelected=false} ) model.Symbols
            |false -> List.map (fun sym -> if [sym.Id] <> sId
                                                then if sym.IsSelected = true
                                                     then { sym with
                                                                InputPorts = List.map (fun port -> {port with PortPos =updatePortPos port.PortPos sym.Pos}) sym.InputPorts
                                                                OutputPorts = List.map (fun port -> {port with PortPos =updatePortPos port.PortPos sym.Pos}) sym.OutputPorts
                                                                Pos = roundSymbolPos sym.Pos
                                                                IsDragging = true
                                                           }
                                                     else sym
                                                else //check whether symbol is selected
                                                    { sym with
                                                        InputPorts = List.map (fun port -> {port with PortPos =updatePortPos port.PortPos sym.Pos}) sym.InputPorts
                                                        OutputPorts = List.map (fun port -> {port with PortPos =updatePortPos port.PortPos sym.Pos}) sym.OutputPorts
                                                        Pos = roundSymbolPos sym.Pos
                                                        IsDragging = true
                                                        // LastDragPos = pagePos
                                                    }
                        ) model.Symbols
        {model with Symbols=snappedSymbols; SingleOrMultiple=isSingleSelected}, Cmd.none//bbouning boxes


    | Dragging (sId, pagePos, prevPagePos) ->
         

        let isSingleSelected =
            List.exists (fun sym -> sId=[sym.Id] && sym.IsSelected = false) model.Symbols
        //if symbol being dragged is not selected - then you are dragging one component

        let diff = posDiff pagePos prevPagePos

        let newPortPos (port:XYPos) = 
            posAdd port diff 

        let dSymbols=
            // let diff = posDiff pagePos prevPagePos//sym.LastDragPos
            match isSingleSelected with
            |true ->printfn "hola %A" model.Symbols
                    List.map (fun sym -> if [sym.Id] = sId
                                         then { sym with 
                                                    Pos = posAdd sym.Pos diff
                                                    IsDragging = true
                                                    LastDragPos = sym.Pos
                                                    InputPorts = List.map (fun port -> {port with PortPos= newPortPos port.PortPos}) sym.InputPorts
                                                    OutputPorts = List.map (fun port -> {port with PortPos= newPortPos port.PortPos}) sym.OutputPorts
                                                }
                                         else {sym with IsSelected=false} ) model.Symbols
            |false -> List.map (fun sym -> if [sym.Id] <> sId
                                                then if sym.IsSelected = true
                                                     then { sym with
                                                                Pos = posAdd sym.Pos diff
                                                                IsDragging = true
                                                                LastDragPos = pagePos
                                                                InputPorts = List.map (fun port -> {port with PortPos = newPortPos port.PortPos}) sym.InputPorts
                                                                OutputPorts = List.map (fun port -> {port with PortPos = newPortPos port.PortPos}) sym.OutputPorts
                                                           }
                                                     else sym
                                                else //check whether symbol is selected
                                                    { sym with
                                                        Pos = posAdd sym.Pos diff
                                                        IsDragging = true
                                                        LastDragPos = pagePos
                                                        InputPorts = List.map (fun port -> {port with PortPos = newPortPos port.PortPos}) sym.InputPorts
                                                        OutputPorts = List.map (fun port -> {port with PortPos = newPortPos port.PortPos}) sym.OutputPorts
                                                    }
                        ) model.Symbols
        let newSymbols, newBox =
            if isSingleSelected = true
            then  List.map2 (fun sym box -> if sId <> [sym.Id]
                                            then (sym, box)
                                            else ({sym with IsDragging=false} , ({X=sym.Pos.X-10.;Y=sym.Pos.Y-10.},{X=sym.Pos.X+sym.W+10.;Y=sym.Pos.Y+sym.H+10.}))) model.Symbols model.SymBBoxes
                  |> List.unzip
            else
                List.map2 (fun sym box -> if sym.IsSelected = false
                                          then (sym, box)
                                          else ({sym with IsDragging = false}, ({X=sym.Pos.X-10.;Y=sym.Pos.Y-10.},{X=sym.Pos.X+sym.W+10.;Y=sym.Pos.Y+sym.H+10.}))) model.Symbols model.SymBBoxes
                |> List.unzip
        printfn "Ports %A diff %A" dSymbols diff 
        {model with Symbols=dSymbols; SingleOrMultiple=isSingleSelected; SymBBoxes = newBox}, Cmd.none

    | UpdateBBoxes (sId) ->
        let newSymbols, newBox =
            if model.SingleOrMultiple = true
            then  List.map2 (fun sym box -> if sId <> [sym.Id]
                                            then (sym, box)
                                            else ({sym with IsDragging=false} , ({X=sym.Pos.X-10.;Y=sym.Pos.Y-10.},{X=sym.Pos.X+sym.W+10.;Y=sym.Pos.Y+sym.H+10.}))) model.Symbols model.SymBBoxes
                  |> List.unzip
            else
                List.map2 (fun sym box -> if sym.IsSelected = false
                                          then (sym, box)
                                          else ({sym with IsDragging = false}, ({X=sym.Pos.X-10.;Y=sym.Pos.Y-10.},{X=sym.Pos.X+sym.W+10.;Y=sym.Pos.Y+sym.H+10.}))) model.Symbols model.SymBBoxes
                |> List.unzip
        // let newBox, newSymbols =
        //     if singleDragBool=true
        //     then
        {model with SymBBoxes=newBox; Symbols=newSymbols}, Cmd.none

    | DeleteSymbol ->
        let (remainingSymbols, remainingBBox) =
             List.fold2 (fun remainingValues sym box -> if sym.IsSelected = false
                                                        then remainingValues @ [(sym, box)]
                                                        else remainingValues ) [] model.Symbols model.SymBBoxes
             |> List.unzip
        {model with Symbols=remainingSymbols; SymBBoxes = remainingBBox}, Cmd.none

        // let selectedList =
        //     let checkSymbol (wiresList, bBoxesList) (wireTest) (boundingBox:XYPos*XYPos)=
        //         if wireTest.IsSelected = true
        //         then (wireTest::wiresList, bBoxesList@[boundingBox])
        //         else (wiresList, bBoxesList)
        //     List.fold2 checkSymbol ([],[]) model.Symbols model.SymBBoxes
        // let remainingSymbols = fst selectedList
        // let remainingBbox = snd selectedList
        // {model with Symbols=remainingSymbols ; SymBBoxes=remainingBbox}, Cmd.none

        //need to do the delete properly
        // let symbolsToKeepIndex (lst:int) = List.filter (fun x -> List.tryFind (fun y -> y = x) sIdList |> function |Some a -> false |None -> true) [0..lst]
        // let dSymbols =
        //      symbolsToKeepIndex ((model.Symbols.Length)- 1)
        //      |> List.map (fun i -> model.Symbols.[i]) // (fun index value ->  List.tryFind (fun x -> x = index) sIdList |> function |Some a -> [] |None -> [value])
        // let dBbox =
        //     symbolsToKeepIndex ((model.SymBBoxes.Length)- 1)
        //     |> List.map (fun i -> model.SymBBoxes.[i])
        // {model with Symbols = dSymbols; SymBBoxes = dBbox}, Cmd.none

    | ToggleSymbol (sId) ->
        let selectedSymbolList =
            List.map (fun sym -> if List.exists (fun idsInList -> idsInList=sym.Id) sId
                                 then {sym with IsSelected=not sym.IsSelected}
                                 else {sym with IsSelected=false}) model.Symbols
        {model with Symbols = selectedSymbolList}, Cmd.none

    |Hovering (sId) ->
        let showPortSymbols =
            model.Symbols
            |> List.map (fun sym -> if [sym.Id] = sId then { sym with PortStatus = Visible}  else { sym with PortStatus = Invisible})
        {model with Symbols = showPortSymbols}, Cmd.none

    |ShowValidPorts (portVis, portId, mousePos) ->
        let validPortSymbols =
            printfn "validPortSymbols %A" portVis
            match (portmove portId portVis model.Symbols) with
            | (symb, port, portNum) -> List.map (fun x -> if x.Id = symb.Id 
                                                          then { x with IsSliding=(portVis, string symb.Id, portNum, mousePos); PortStatus=portVis}  
                                                          else { x with PortStatus=portVis; IsSliding = (portVis, string symb.Id, portNum, mousePos)}) model.Symbols
        {model with Symbols =  validPortSymbols}, Cmd.none

    | MouseMsg {Pos = {X=posX; Y=posY}; Op = Down} ->
        let showPorts =
            model.Symbols
            |> List.map (fun x -> { x with PortStatus=Invisible; IsSliding=(ShowInputsOnly , "null", 0, {X=0.; Y=0.})})
        {model with Symbols = showPorts}, Cmd.none
    | _ -> failwith "Not Implemented, Symbol Update Function, Symbol line 299" // allow unused mouse messags


type private RenderSymbolProps =
    {
        Symb : Symbol // name works for the demo!
        Dispatch : Dispatch<Msg>
        key: string // special field used by react to detect whether lists have changed, set to symbol Id
        Comp : CommonTypesOne.ComponentType
    }

/// View for one symbol with caching for efficient execution when input does not change



let private RenderSymbol (comp: CommonTypesOne.ComponentType)=

    match comp with
    // | Input bits | Output bits ->
    //     FunctionComponent.Of(
    //         fun (props : RenderSymbolProps) ->
    //             let color =
    //                 if props.Symb.IsDragging then
    //                     "green"
    //                 else
    //                     "grey"
    //             g   []
    //                 [
    //                     rectum props.Symb.Pos.X props.Symb.Pos.Y (gateWidth/3.) (gateHeight/4.) color

    //                 ]
    //     )
    | Not | And | Or | Xor | Nand | Nor | Xnor->
        FunctionComponent.Of(
            fun (props : RenderSymbolProps) ->
                // let handleMouseMove =
                //     Hooks.useRef(fun (ev : Types.Event) ->
                //         let ev = ev :?> Types.MouseEvent
                //         // x,y coordinates here do not compensate for transform in Sheet
                //         // and are wrong unless zoom=1.0 MouseMsg coordinates are correctly compensated.
                //         Dragging([props.Symb.Id], (posOf ev.pageX ev.pageY))
                //         |> props.Dispatch
                //     )
                let (portVis, symId, portNum, mousePosition) = props.Symb.IsSliding
                let displayPort = 
                    let outputPorts = List.map (fun (ports:CommonTypesOne.Port) -> circus ports.PortPos.X  ports.PortPos.Y 5. ) props.Symb.OutputPorts
                    let inputPorts = List.map (fun (ports:CommonTypesOne.Port) -> circus ports.PortPos.X  ports.PortPos.Y 5. ) props.Symb.InputPorts
                    let slideCirc IO portNum (mousePos:XYPos)=
                        let portList =
                            if IO = "input" then props.Symb.InputPorts.[portNum].PortPos else props.Symb.OutputPorts.[portNum].PortPos
                        [
                            circus portList.X  portList.Y 5.
                            line [
                                X1 portList.X
                                Y1 portList.Y
                                X2 mousePos.X
                                Y2 mousePos.Y
                                SVGAttr.StrokeDasharray "4"
                                // Qualify these props to avoid name collision with CSSProp
                                SVGAttr.Stroke "black"
                                SVGAttr.StrokeWidth 5 ] []
                        ]
                
                    match props.Symb.PortStatus with
                    | Visible ->    outputPorts @ inputPorts
                
                    | Invisible ->  []
                
                    | ShowInputsOnly -> if string props.Symb.Id = symId 
                                        then slideCirc "output" portNum mousePosition 
                                        else inputPorts
                
                    | ShowOutputsOnly -> if string props.Symb.Id = symId 
                                         then slideCirc "input" portNum mousePosition 
                                         else outputPorts

                let color =
                    if props.Symb.IsSelected then
                        "green"
                    else
                        "grey"

                let mainOutline = 
                      
                        let textSection =   match props.Comp with
                                            | Not -> [homotextual (props.Symb.Pos.X + gateWidth/2.)  (props.Symb.Pos.Y + gateHeight/8.) "middle" "Middle" "14px" "1"]
                                                     |> List.append [circus (props.Symb.Pos.X + gateWidth + circleRadius) (props.Symb.Pos.Y + gateHeight/2.) circleRadius]
                                                     |> List.append [homotextual (props.Symb.Pos.X + gateWidth - inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/2.) "end" "Middle" "10px" "Y0"]
                                                     |> List.append [homotextual (props.Symb.Pos.X + inOutLines*0.5 ) (props.Symb.Pos.Y + gateHeight/2.) "start" "Middle" "10px" "X0"]
                                                     |> List.append [creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/2.) props.Symb.Pos.X (props.Symb.Pos.Y + gateHeight/2.) 2]
                                                     |> List.append [creditLines (props.Symb.Pos.X + gateWidth + inOutLines) (props.Symb.Pos.Y + gateHeight/2.) (props.Symb.Pos.X + gateWidth + 2.*inOutLines) (props.Symb.Pos.Y + gateHeight/2.) 2]

                                            | And -> [homotextual (props.Symb.Pos.X + gateWidth/2.)  (props.Symb.Pos.Y + gateHeight/8.) "middle" "Middle" "14px" "&"]
                                                     |> List.append [homotextual (props.Symb.Pos.X + gateWidth - inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/2.) "end" "Middle" "10px" "Y0"]
                                                     |> List.append [homotextual (props.Symb.Pos.X + inOutLines*0.5 ) (props.Symb.Pos.Y + gateHeight/4.) "start" "Middle" "10px" "X0"]
                                                     |> List.append [creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/4.) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/4.) 2]
                                                     |> List.append [homotextual (props.Symb.Pos.X + inOutLines*0.5 ) (props.Symb.Pos.Y + gateHeight/(4./3.)) "start" "Middle" "10px" "X1"]
                                                     |> List.append [creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/(4./3.)) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/(4./3.)) 2]
                                                     |> List.append [creditLines (props.Symb.Pos.X + gateWidth) (props.Symb.Pos.Y + gateHeight/2.) (props.Symb.Pos.X + gateWidth + inOutLines) (props.Symb.Pos.Y + gateHeight/2.) 2]
                                            | Or -> [homotextual (props.Symb.Pos.X + gateWidth/2.)  (props.Symb.Pos.Y + gateHeight/8.) "middle" "Middle" "14px" "≥1"]
                                                    |> List.append [homotextual (props.Symb.Pos.X + gateWidth - inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/2.) "end" "Middle" "10px" "Y0"]
                                                    |> List.append [homotextual (props.Symb.Pos.X + inOutLines*0.5 ) (props.Symb.Pos.Y + gateHeight/4.) "start" "Middle" "10px" "X0"]
                                                    |> List.append [creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/4.) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/4.) 2]
                                                    |> List.append [homotextual (props.Symb.Pos.X + inOutLines*0.5 ) (props.Symb.Pos.Y + gateHeight/(4./3.)) "start" "Middle" "10px" "X1"]
                                                    |> List.append [creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/(4./3.)) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/(4./3.)) 2]
                                                    |> List.append [creditLines (props.Symb.Pos.X + gateWidth) (props.Symb.Pos.Y + gateHeight/2.) (props.Symb.Pos.X + gateWidth + inOutLines) (props.Symb.Pos.Y + gateHeight/2.) 2]
                                            | Xor -> [homotextual (props.Symb.Pos.X + gateWidth/2.)  (props.Symb.Pos.Y + gateHeight/8.) "middle" "Middle" "14px" "=1"]
                                                     |> List.append [homotextual (props.Symb.Pos.X + gateWidth - inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/2.) "end" "Middle" "10px" "Y0"]
                                                     |> List.append [homotextual (props.Symb.Pos.X + inOutLines*0.5 ) (props.Symb.Pos.Y + gateHeight/4.) "start" "Middle" "10px" "X0"]
                                                     |> List.append [creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/4.) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/4.) 2]
                                                     |> List.append [homotextual (props.Symb.Pos.X + inOutLines*0.5 ) (props.Symb.Pos.Y + gateHeight/(4./3.)) "start" "Middle" "10px" "X1"]
                                                     |> List.append [creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/(4./3.)) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/(4./3.)) 2]
                                                     |> List.append [creditLines (props.Symb.Pos.X + gateWidth) (props.Symb.Pos.Y + gateHeight/2.) (props.Symb.Pos.X + gateWidth + inOutLines) (props.Symb.Pos.Y + gateHeight/2.) 2]
                                            | Nand -> [homotextual (props.Symb.Pos.X + gateWidth/2.)  (props.Symb.Pos.Y + gateHeight/8.) "middle" "Middle" "14px" "&"]
                                                      |> List.append [circus (props.Symb.Pos.X + gateWidth + circleRadius) (props.Symb.Pos.Y + gateHeight/2.) circleRadius]
                                                      |> List.append [homotextual (props.Symb.Pos.X + gateWidth - inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/2.) "end" "Middle" "10px" "Y0"]
                                                      |> List.append [homotextual (props.Symb.Pos.X + inOutLines*0.5 ) (props.Symb.Pos.Y + gateHeight/4.) "start" "Middle" "10px" "X0"]
                                                      |> List.append [creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/4.) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/4.) 2]
                                                      |> List.append [homotextual (props.Symb.Pos.X + inOutLines*0.5 ) (props.Symb.Pos.Y + gateHeight/(4./3.)) "start" "Middle" "10px" "X1"]
                                                      |> List.append [creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/(4./3.)) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/(4./3.)) 2]
                                                      |> List.append [creditLines (props.Symb.Pos.X + gateWidth + inOutLines) (props.Symb.Pos.Y + gateHeight/2.) (props.Symb.Pos.X + gateWidth + 2.*inOutLines) (props.Symb.Pos.Y + gateHeight/2.) 2]
                                            | Nor -> [homotextual (props.Symb.Pos.X + gateWidth/2.)  (props.Symb.Pos.Y + gateHeight/8.) "middle" "Middle" "14px" "≥1"]
                                                     |> List.append [circus (props.Symb.Pos.X + gateWidth + circleRadius) (props.Symb.Pos.Y + gateHeight/2.) circleRadius]
                                                     |> List.append [homotextual (props.Symb.Pos.X + gateWidth - inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/2.) "end" "Middle" "10px" "Y0"]
                                                     |> List.append [homotextual (props.Symb.Pos.X + inOutLines*0.5 ) (props.Symb.Pos.Y + gateHeight/4.) "start" "Middle" "10px" "X0"]
                                                     |> List.append [creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/4.) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/4.) 2]
                                                     |> List.append [homotextual (props.Symb.Pos.X + inOutLines*0.5 ) (props.Symb.Pos.Y + gateHeight/(4./3.)) "start" "Middle" "10px" "X1"]
                                                     |> List.append [creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/(4./3.)) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/(4./3.)) 2]
                                                     |> List.append [creditLines (props.Symb.Pos.X + gateWidth + inOutLines) (props.Symb.Pos.Y + gateHeight/2.) (props.Symb.Pos.X + gateWidth + 2.*inOutLines) (props.Symb.Pos.Y + gateHeight/2.) 2]
                                            | Xnor -> [homotextual (props.Symb.Pos.X + gateWidth/2.)  (props.Symb.Pos.Y + gateHeight/8.) "middle" "Middle" "14px" "=1"]
                                                      |> List.append [circus (props.Symb.Pos.X + gateWidth + circleRadius) (props.Symb.Pos.Y + gateHeight/2.) circleRadius]
                                                      |> List.append [homotextual (props.Symb.Pos.X + gateWidth - inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/2.) "end" "Middle" "10px" "Y0"]
                                                      |> List.append [homotextual (props.Symb.Pos.X + inOutLines*0.5 ) (props.Symb.Pos.Y + gateHeight/4.) "start" "Middle" "10px" "X0"]
                                                      |> List.append [creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/4.) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/4.) 2]
                                                      |> List.append [homotextual (props.Symb.Pos.X + inOutLines*0.5 ) (props.Symb.Pos.Y + gateHeight/(4./3.)) "start" "Middle" "10px" "X1"]
                                                      |> List.append [creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/(4./3.)) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/(4./3.)) 2]
                                                      |> List.append [creditLines (props.Symb.Pos.X + gateWidth + inOutLines) (props.Symb.Pos.Y + gateHeight/2.) (props.Symb.Pos.X + gateWidth + 2.*inOutLines) (props.Symb.Pos.Y + gateHeight/2.) 2]
                                            | _ -> [homotextual 0 0 "" "" "" ""]

                        textSection @ [rectum props.Symb.Pos.X props.Symb.Pos.Y gateWidth gateHeight color props]
                   

                let finalPortsSymbol = mainOutline @ displayPort 

                g   [
                        // OnMouseUp (fun ev ->
                        //     document.removeEventListener("mousemove", handleMouseMove.current)
                        //     // EndDragging props.Square.Id
                        //     // |> props.Dispatch
                        // )
                        // OnMouseDown (fun ev ->
                        //     // See note above re coords wrong if zoom <> 1.0
                        //     // StartDragging (props.Square.Id, posOf ev.pageX ev.pageY)
                        //     // |> props.Dispatch
                        //     document.addEventListener("mousemove", handleMouseMove.current)
                        // )
                    ] (finalPortsSymbol)


        )
    | Mux2 | Demux2 ->
        FunctionComponent.Of(
            fun (props : RenderSymbolProps) ->

                let color =
                    if props.Symb.IsDragging then
                        "green"
                    else
                        "grey"
                g   [
                    ]
                    [
                        rectum props.Symb.Pos.X props.Symb.Pos.Y (gateWidth*2.) gateHeight color props

                        match props.Comp with
                        | Mux2 ->
                            homotextual (props.Symb.Pos.X + gateWidth) (props.Symb.Pos.Y + gateHeight/8.) "middle" "middle" "14px" "MUX"

                            homotextual (props.Symb.Pos.X + inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/2.) "start" "Middle" "10px" "X1"
                            creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/2.) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/2.) 2

                            homotextual (props.Symb.Pos.X + gateWidth*2. - inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/2.) "end" "Middle" "10px" "Y0"
                            creditLines (props.Symb.Pos.X + gateWidth*2. + inOutLines) (props.Symb.Pos.Y + gateHeight/2.) (props.Symb.Pos.X + gateWidth*2.) (props.Symb.Pos.Y + gateHeight/2.) 2//Mux output



                        | Demux2 ->
                            homotextual (props.Symb.Pos.X + gateWidth) (props.Symb.Pos.Y + gateHeight/8.) "middle" "middle" "14px" "DEMUX"

                            homotextual (props.Symb.Pos.X + gateWidth*2. - inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/4.) "end" "Middle" "10px" "Y0"
                            creditLines (props.Symb.Pos.X + gateWidth*2. + inOutLines) (props.Symb.Pos.Y + gateHeight/4.) (props.Symb.Pos.X + gateWidth*2.) (props.Symb.Pos.Y + gateHeight/4.) 2//Mux output

                            homotextual (props.Symb.Pos.X + gateWidth*2. - inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/(4./3.)) "end" "Middle" "10px" "Y1"
                            creditLines (props.Symb.Pos.X + gateWidth*2. + inOutLines) (props.Symb.Pos.Y + gateHeight/(4./3.)) (props.Symb.Pos.X + gateWidth*2.) (props.Symb.Pos.Y + gateHeight/(4./3.)) 2//Mux output

                        | _ ->
                            homotextual 0 0 "" "" "" ""

                        homotextual (props.Symb.Pos.X + inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/4.) "start" "middle" "10px" "X0"
                        creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/4.) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/4.) 2

                        homotextual (props.Symb.Pos.X + inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/(4./3.)) "start" "middle" "10px" "EN"
                        creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/(4./3.)) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/(4./3.)) 2


                    ]

        )
    | NbitsAdder bits ->
            FunctionComponent.Of(
                fun (props : RenderSymbolProps) ->

                    let color =
                        if props.Symb.IsDragging then
                            "green"
                        else
                            "grey"
                    g   [
                        ]
                        [
                            rectum props.Symb.Pos.X props.Symb.Pos.Y (gateWidth*2.) gateHeight color props

                            text
                                [   X (props.Symb.Pos.X + gateWidth)
                                    Y (props.Symb.Pos.Y + gateHeight/8.)
                                    Style
                                        [
                                                TextAnchor "middle"
                                                DominantBaseline "middle"
                                                FontSize "14px"
                                                FontWeight "Bold"
                                                Fill "Black"
                                            ]
                                ]
                                [ str <| sprintf "Adder(%A:0)" bits ]

                            //inputs
                            homotextual (props.Symb.Pos.X + inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/4.) "start" "middle" "10px" "Cin"
                            creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/4.) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/4.) 2

                            homotextual (props.Symb.Pos.X + inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/(4./3.)) "start" "middle" "10px" "A"
                            creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/(4./3.)) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/(4./3.)) 2

                            homotextual (props.Symb.Pos.X + inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/2.) "start" "Middle" "10px" "B"
                            creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/2.) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/2.) 2

                            //outputa
                            homotextual (props.Symb.Pos.X + gateWidth*2. - inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/3.) "end" "Middle" "10px" "Sum"
                            creditLines (props.Symb.Pos.X + gateWidth*2. + inOutLines) (props.Symb.Pos.Y + gateHeight/3.) (props.Symb.Pos.X + gateWidth*2.) (props.Symb.Pos.Y + gateHeight/3.) 2//Mux output

                            homotextual (props.Symb.Pos.X + gateWidth*2. - inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/(3./2.)) "end" "Middle" "10px" "Cout"
                            creditLines (props.Symb.Pos.X + gateWidth*2. + inOutLines) (props.Symb.Pos.Y + gateHeight/(3./2.)) (props.Symb.Pos.X + gateWidth*2.) (props.Symb.Pos.Y + gateHeight/(3./2.)) 2//Mux output


                            ]

        )
    | MergeWires ->
        FunctionComponent.Of(
            fun (props : RenderSymbolProps) ->
                let color =
                    if props.Symb.IsDragging then
                        "green"
                    else
                        "grey"
                g   [
                    ]
                    [   // first pure vertical line
                        creditLines (props.Symb.Pos.X) (props.Symb.Pos.Y) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/3.) 2
                        // Second horizontal line top of shape
                        creditLines (props.Symb.Pos.X) (props.Symb.Pos.Y) (props.Symb.Pos.X - gateWidth/3.) (props.Symb.Pos.Y) 2
                        // Third horizontal line bottom of shape
                        creditLines (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/3.) (props.Symb.Pos.X - gateWidth/3.) (props.Symb.Pos.Y + gateHeight/3.) 2
                        // 4th line in the middle; output line
                        creditLines (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/6.) (props.Symb.Pos.X + gateWidth/3.) (props.Symb.Pos.Y + gateHeight/6.) 2


                        ]
        )
    | SplitWire bits ->
        FunctionComponent.Of(
            fun (props : RenderSymbolProps) ->

                let color =
                    if props.Symb.IsDragging then
                        "green"
                    else
                        "grey"
                g   [
                    ]

                    [   //let formatted = String.Format ("(1:{0})", bits) wtf

                    //    homotextual (props.Symb.Pos.X) (props.Symb.Pos.Y - gateHeight/8.) "middle" "middle" "14px" formatted
                        text
                            [ X (props.Symb.Pos.X )
                              Y (props.Symb.Pos.Y - gateHeight/8.)
                              Style
                                    [
                                         TextAnchor "middle"
                                         DominantBaseline "middle"
                                         FontSize "14px"
                                         FontWeight "Bold"
                                         Fill "Black"
                                     ]
                            ]
                            [str <| sprintf "(1:%A)" bits]


                        // first pure vertical line
                        creditLines (props.Symb.Pos.X) (props.Symb.Pos.Y) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/3.) 2
                        // Second horizontal line top of shape
                        creditLines (props.Symb.Pos.X) (props.Symb.Pos.Y) (props.Symb.Pos.X + gateWidth/3.) (props.Symb.Pos.Y) 2
                        // Third horizontal line bottom of shape
                        creditLines (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/3.) (props.Symb.Pos.X + gateWidth/3.) (props.Symb.Pos.Y + gateHeight/3.) 2
                        // 4th line in the middle; output line
                        creditLines (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/6.) (props.Symb.Pos.X - gateWidth/3.) (props.Symb.Pos.Y + gateHeight/6.) 2



                        ]



        )
    | DFF | DFFE ->
        FunctionComponent.Of(
            fun (props : RenderSymbolProps) ->

                let color =
                    if props.Symb.IsDragging then
                        "green"
                    else
                        "grey"
                g   [
                    ]
                    [
                        rectum props.Symb.Pos.X props.Symb.Pos.Y (gateWidth*2.) gateHeight color props


                        match props.Comp with
                        | DFF ->
                            homotextual (props.Symb.Pos.X + gateWidth) (props.Symb.Pos.Y + gateHeight/8.) "middle" "middle" "14px" "DFF"


                        | DFFE ->
                            homotextual (props.Symb.Pos.X + gateWidth) (props.Symb.Pos.Y + gateHeight/8.) "middle" "middle" "14px" "DFFE"

                            homotextual (props.Symb.Pos.X + inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/2.) "start" "middle" "10px" "EN"
                            creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/2.) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/2.) 2

                        | _ ->
                            homotextual 0 0 "" "" "" ""

                        homotextual (props.Symb.Pos.X + inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/4.) "start" "middle" "10px" "D"
                        creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/4.) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/4.) 2

                        homotextual (props.Symb.Pos.X + gateWidth*2. - inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/4.) "end" "middle" "10px" "Q"
                        creditLines (props.Symb.Pos.X + gateWidth*2. + inOutLines) (props.Symb.Pos.Y + gateHeight/4.) (props.Symb.Pos.X + gateWidth*2.) (props.Symb.Pos.Y + gateHeight/4.) 2//Mux output


                        homotextual (props.Symb.Pos.X + inOutLines*1.1) (props.Symb.Pos.Y + gateHeight/(20./17.)) "start" "middle" "9px" "Clk"
                        creditLines (props.Symb.Pos.X + inOutLines) (props.Symb.Pos.Y + gateHeight/(20./17.)) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/(5./4.)) 1 // CLK
                        creditLines (props.Symb.Pos.X + inOutLines) (props.Symb.Pos.Y + gateHeight/(20./17.)) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/(10./9.)) 1 // CLK


                ]

        )
    | Register bits | RegisterE bits->
        FunctionComponent.Of(
            fun (props : RenderSymbolProps) ->

                let color =
                    if props.Symb.IsDragging then
                        "green"
                    else
                        "grey"
                g   [
                    ]
                    [
                        rectum props.Symb.Pos.X props.Symb.Pos.Y (gateWidth*2.) gateHeight color props


                        match props.Comp with
                        | Register bits ->
                            homotextual (props.Symb.Pos.X + gateWidth) (props.Symb.Pos.Y + gateHeight/8.) "middle" "middle" "14px" "Register"



                        | RegisterE bits ->
                            homotextual (props.Symb.Pos.X + gateWidth) (props.Symb.Pos.Y + gateHeight/8.) "middle" "middle" "14px" "RegisterE"

                            homotextual (props.Symb.Pos.X + inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/2.) "start" "middle" "10px" "EN"
                            creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/2.) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/2.) 2

                        | _ ->
                            homotextual 0 0 "" "" "" ""

                        homotextual (props.Symb.Pos.X + inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/4.) "start" "middle" "10px" "Data-in"
                        creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/4.) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/4.) 2

                        homotextual (props.Symb.Pos.X + gateWidth*2. - inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/4.) "end" "middle" "10px" "Data-out"
                        creditLines (props.Symb.Pos.X + gateWidth*2. + inOutLines) (props.Symb.Pos.Y + gateHeight/4.) (props.Symb.Pos.X + gateWidth*2.) (props.Symb.Pos.Y + gateHeight/4.) 2//Mux output


                        homotextual (props.Symb.Pos.X + inOutLines*1.1) (props.Symb.Pos.Y + gateHeight/(20./17.)) "start" "middle" "9px" "Clk"
                        creditLines (props.Symb.Pos.X + inOutLines) (props.Symb.Pos.Y + gateHeight/(20./17.)) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/(5./4.)) 1 // CLK
                        creditLines (props.Symb.Pos.X + inOutLines) (props.Symb.Pos.Y + gateHeight/(20./17.)) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/(10./9.)) 1 // CLK



                ]

        )
    | ROM memorySize | AsyncROM memorySize ->
        FunctionComponent.Of(
            fun (props : RenderSymbolProps) ->

                let color =
                    if props.Symb.IsDragging then
                        "green"
                    else
                        "grey"
                g   [
                    ]
                    [
                        rectum props.Symb.Pos.X props.Symb.Pos.Y (gateWidth*2.) gateHeight color props

                        match props.Comp with
                        | ROM memorySize ->
                            homotextual (props.Symb.Pos.X + gateWidth) (props.Symb.Pos.Y + gateHeight/8.) "middle" "middle" "14px" "ROM"

                            homotextual (props.Symb.Pos.X + inOutLines*1.1) (props.Symb.Pos.Y + gateHeight/(20./17.)) "start" "middle" "9px" "Clk"
                            line [ X1 (props.Symb.Pos.X + inOutLines); Y1 (props.Symb.Pos.Y + gateHeight/(20./17.)); X2 (props.Symb.Pos.X); Y2 (props.Symb.Pos.Y + gateHeight/(5./4.)); Style[CSSProp.Stroke "Black" ; CSSProp.StrokeWidth 1]] []  //clk ting
                            line [ X1 (props.Symb.Pos.X + inOutLines); Y1 (props.Symb.Pos.Y + gateHeight/(20./17.)); X2 (props.Symb.Pos.X); Y2 (props.Symb.Pos.Y + gateHeight/(10./9.)); Style[CSSProp.Stroke "Black" ; CSSProp.StrokeWidth 1]] []  //clk ting




                        | AsyncROM memorySize ->
                            homotextual (props.Symb.Pos.X + gateWidth) (props.Symb.Pos.Y + gateHeight/8.) "middle" "middle" "14px" "AsyncROM"


                        | _ ->
                            homotextual 0 0 "" "" "" ""


                        homotextual (props.Symb.Pos.X + inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/2.) "start" "middle" "10px" "Addr"
                        creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/2.) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/2.) 2 // CLK



                        homotextual (props.Symb.Pos.X + gateWidth*2. - inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/2.) "end" "middle" "10px" "Data-out"
                        creditLines (props.Symb.Pos.X + gateWidth*2. + inOutLines) (props.Symb.Pos.Y + gateHeight/2.) (props.Symb.Pos.X + gateWidth*2.) (props.Symb.Pos.Y + gateHeight/2.) 2 // CLK



                ]

        )
    | RAM memorySize ->
        FunctionComponent.Of(
            fun (props : RenderSymbolProps) ->

                let color =
                    if props.Symb.IsDragging then
                        "green"
                    else
                        "grey"
                g   [
                    ]
                    [
                        rectum props.Symb.Pos.X props.Symb.Pos.Y (gateWidth*2.) gateHeight color props

                        homotextual (props.Symb.Pos.X + gateWidth) (props.Symb.Pos.Y + gateHeight/8.) "middle" "middle" "14px" "RAM"


                        homotextual (props.Symb.Pos.X + inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/6.) "start" "middle" "10px" "Addr"
                        creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/6.) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/6.) 2 // CLK


                        homotextual (props.Symb.Pos.X + inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/3.) "start" "middle" "10px" "Data-in"
                        creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/3.) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/3.) 2 // CLK

                        homotextual (props.Symb.Pos.X + inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/2.) "start" "middle" "10px" "Write"
                        creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/2.) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/2.) 2 // CLK


                        homotextual (props.Symb.Pos.X + gateWidth*2. - inOutLines*0.5) (props.Symb.Pos.Y + gateHeight/2.) "end" "middle" "10px" "Data-out"
                        creditLines (props.Symb.Pos.X + gateWidth*2. + inOutLines) (props.Symb.Pos.Y + gateHeight/2.) (props.Symb.Pos.X + gateWidth*2.) (props.Symb.Pos.Y + gateHeight/2.) 2 // CLK


                        homotextual (props.Symb.Pos.X + inOutLines*1.1) (props.Symb.Pos.Y + gateHeight/(20./17.)) "start" "middle" "9px" "Clk"
                        line [ X1 (props.Symb.Pos.X + inOutLines); Y1 (props.Symb.Pos.Y + gateHeight/(20./17.)); X2 (props.Symb.Pos.X); Y2 (props.Symb.Pos.Y + gateHeight/(5./4.)); Style[CSSProp.Stroke "Black" ; CSSProp.StrokeWidth 1]] []  //clk ting
                        line [ X1 (props.Symb.Pos.X + inOutLines); Y1 (props.Symb.Pos.Y + gateHeight/(20./17.)); X2 (props.Symb.Pos.X); Y2 (props.Symb.Pos.Y + gateHeight/(10./9.)); Style[CSSProp.Stroke "Black" ; CSSProp.StrokeWidth 1]] []  //clk ting

                ]
        )
    | Custom customSymbol ->    //custom symbol contains - name of component; list of input and output ports with name & buswidth
            //generate ports
            
         FunctionComponent.Of(
            fun (props : RenderSymbolProps) ->
                let color =
                    if props.Symb.IsSelected then
                        "green"
                    else
                        "grey"
                let generatePortsList index portName inoutput =
                    let inputPortNum = List.length props.Symb.InputPorts
                    let outputPortNum = List.length props.Symb.OutputPorts
                    let inputPortName = Printf.StringFormat<string> (fst customSymbol.InputLabels.[index]) 
                    // let outputPortName = Printf.StringFormat<string> (fst customSymbol.OutputLabels.[index]) 
                    match inoutput with 
                    | true -> (homotextual (props.Symb.Pos.X + inOutLines*0.5 ) (props.Symb.Pos.Y + ((float index + 1.)/((float inputPortNum) + 1.)*props.Symb.H)) "start" "Middle" "10px" inputPortName),
                              (creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + ((float index + 1.)/((float inputPortNum) + 1.)*props.Symb.H)) (props.Symb.Pos.X) (props.Symb.Pos.Y + (float(index + 1)/(float inputPortNum + 1.)*props.Symb.H)) 2) 

                    | false -> (homotextual (props.Symb.Pos.X + gateWidth - inOutLines*1.7) (props.Symb.Pos.Y + (float(index + 1)/(float outputPortNum + 1.)*props.Symb.H)) "start" "Middle" "10px" "op"),
                               (creditLines (props.Symb.Pos.X + gateWidth + inOutLines) (props.Symb.Pos.Y + (float (index + 1)/(float outputPortNum + 1.)*props.Symb.H)) (props.Symb.Pos.X + gateWidth) (props.Symb.Pos.Y + (float (index + 1)/(float outputPortNum + 1.)*props.Symb.H)) 2 )

                let generateSVGChild =
                    let standard  =
                        [
                            //shape - done
                            rectum props.Symb.Pos.X props.Symb.Pos.Y gateWidth props.Symb.H color props //can do gateHeight*max inputs/outputs

                            //name of component - potentially change width based on name size - done
                            let name = Printf.StringFormat<string> customSymbol.Name                                     
                            homotextual (props.Symb.Pos.X + gateWidth/2.)  (props.Symb.Pos.Y + props.Symb.H/8.) "middle" "Middle" "14px" name
                        ]      
                    List.mapi (fun index (portName,_) -> generatePortsList index portName true) customSymbol.InputLabels
                    |> List.append (List.mapi (fun index (portName,_) -> generatePortsList index portName false) customSymbol.OutputLabels)
                    |> List.unzip
                    |> (fun (reactList1, reactList2) -> reactList1@reactList2)
                    |> List.append standard


                g   [
                    ](generateSVGChild)

            )

    |_-> failwithf"not yet implemented"







//----------------------------View Function for Symbols----------------------------//

/// Input to react component (which does not re-evaluate when inputs stay the same)
/// This generates View (react virtual DOM SVG elements) for one symbol
type private RenderCircleProps =
    {
        Circle : Symbol // name works for the demo!
        Dispatch : Dispatch<Msg>
        key: string // special field used by react to detect whether lists have changed, set to symbol Id
    }

/// View for one symbol with caching for efficient execution when input does not change
/// View function for symbol layer of SVG



let view (model : Model) (dispatch : Msg -> unit) =
    model.Symbols
    |> List.map (fun rect ->
        RenderSymbol rect.Type
            {
            Symb = rect // name works for the demo!
            Dispatch = dispatch
            key= (string) rect.Id // special field used by react to detect whether lists have changed, set to symbol Id
            Comp = rect.Type
            }
    )
    |> ofList

//---------------Other interface functions--------------------//

let symbolPos (symModel: Model) (sId: CommonTypesOne.ComponentId) : XYPos =
    List.find (fun sym -> sym.Id = sId) symModel.Symbols
    |> (fun sym -> sym.Pos)

let inputPortList (symModel: Model) (sId: CommonTypesOne.ComponentId) : CommonTypesOne.Port list =
    List.find (fun sym -> sym.Id = sId) symModel.Symbols
    |> (fun sym -> sym.InputPorts)

let outputPortList (symModel: Model) (sId: CommonTypesOne.ComponentId) : CommonTypesOne.Port list =
    List.find (fun sym -> sym.Id = sId) symModel.Symbols
    |> (fun sym -> sym.OutputPorts)

let inputPortPos (symModel: Model) (sId: CommonTypesOne.ComponentId) (pId: CommonTypesOne.InputPortId) : XYPos =
    let portList = inputPortList symModel sId

    List.find (fun (por:CommonTypesOne.Port) -> por.Id = string pId) portList
    |> (fun por -> por.PortPos)

let outputPortPos (symModel: Model) (sId: CommonTypesOne.ComponentId) (pId: CommonTypesOne.OutputPortId) : XYPos =
    let portList = outputPortList symModel sId

    List.find (fun (por:CommonTypesOne.Port) -> por.Id = string pId) portList
    |> (fun por -> por.PortPos)

/// Update the symbol with matching componentId to comp, or add a new symbol based on comp.
let updateSymbolModelWithComponent (symModel: Model) (comp:CommonTypesOne.Component) =
    failwithf "Not Implemented"

/// Return the output Buswire width (in bits) if this can be calculated based on known
/// input wire widths, for the symbol wId. The types used here are possibly wrong, since
/// this calculation is based on ports, and the skeleton code does not implement ports or
/// port ids. If This is done the inputs could be expressed in terms of port Ids.
let calculateOutputWidth
        (wId: CommonTypesOne.ConnectionId)
        (outputPortNumber: int)
        (inputPortWidths: int option list) : int option =
    failwithf "Not implemented"


//----------------------interface to Issie-----------------------------//
let extractComponent
        (symModel: Model)
        (sId:CommonTypesOne.ComponentId) : CommonTypesOne.Component =
    failwithf "Not implemented"

let extractComponents (symModel: Model) : CommonTypesOne.Component list =
    failwithf "Not implemented"


    

//match props.Comp with
//     | Nor | Not | Nand | Xnor ->
//         circus (props.Symb.Pos.X + gateWidth + circleRadius) (props.Symb.Pos.Y + gateHeight/2.) circleRadius
//     | _ -> circus 0 0 0

//match props.Comp with
//| Not ->
//    homotextual (props.Symb.Pos.X + inOutLines*0.5 ) (props.Symb.Pos.Y + gateHeight/2.) "start" "Middle" "10px" "X0"
//    creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/2.) props.Symb.Pos.X (props.Symb.Pos.Y + gateHeight/2.) 2
//    // Seq.ofList (renderPorts Visible 1 props.Symb)
//| _ ->
//    homotextual (props.Symb.Pos.X + inOutLines*0.5 ) (props.Symb.Pos.Y + gateHeight/4.) "start" "Middle" "10px" "X0"
//    creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/4.) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/4.) 2
//    // renderPorts Visible ((List.length props.Symb.OutputPorts)-1) props.Symb
//    homotextual (props.Symb.Pos.X + inOutLines*0.5 ) (props.Symb.Pos.Y + gateHeight/(4./3.)) "start" "Middle" "10px" "X1"
//    creditLines (props.Symb.Pos.X - inOutLines) (props.Symb.Pos.Y + gateHeight/(4./3.)) (props.Symb.Pos.X) (props.Symb.Pos.Y + gateHeight/(4./3.)) 2


//match props.Comp with
//| And | Or | Xor ->
//    creditLines (props.Symb.Pos.X + gateWidth) (props.Symb.Pos.Y + gateHeight/2.) (props.Symb.Pos.X + gateWidth + inOutLines) (props.Symb.Pos.Y + gateHeight/2.) 2
//| _ ->
//    creditLines (props.Symb.Pos.X + gateWidth + inOutLines) (props.Symb.Pos.Y + gateHeight/2.) (props.Symb.Pos.X + gateWidth + 2.*inOutLines) (props.Symb.Pos.Y + gateHeight/2.) 2

//let renderPorts (portVisibility:PortVisibility) num sym =


//renderPorts Visible ((List.length props.Symb.OutputPorts)-1) props.Symb
        // // let individiualPorts =
//     let (slide, IO, slidePortNum, {X=xSlide; Y = ySlide}) = sym.IsSliding
//     let slideCirc =
//         let portList =
//             if IO = "input" then sym.InputPorts.[(int num)].PortPos
//             else sym.OutputPorts.[(int num)].PortPos
//         [
//                  circus xSlide ySlide 5.
//                  line [
//                      X1 portList.X   //fst portList)
//                      Y1 portList.Y   //(snd portList)
//                      X2 xSlide
//                      Y2 ySlide
//                      SVGAttr.StrokeDasharray "4"
//                      // Qualify these props to avoid name collision with CSSProp
//                      SVGAttr.Stroke "black"
//                      SVGAttr.StrokeWidth 5 ] []
//         ]
//
//     let portSection =
//         match (portVisibility, slide, num, IO) with  // which port status, in or out side we need to print, whether the rectangle moves, port number, input or output port that slides
//         |("visible", _, _, _ ) -> inPorts @ outPorts
//         |(_, true, slidePortNum, _) -> slideCirc // for valid ports but the port that slides for a sliding output
//         |(_, true, slidePortNum, "output") -> inPorts
//         |(_, true, slidePortNum, "input") -> outPorts//for normal showing ports when nearby
//         |("input",false, _,_) -> outPorts //for valid ports but no sliding so if input state then show the available outputs
//         |("output", false,_,_ ) -> inPorts
//         |_ -> []

//     match portVisibility with
//     |"invisible" -> []
//     |_ -> [portSection]




















