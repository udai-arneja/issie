(*
    PopupView.fs

    This module provides a handy interface to create popups and notifications.
    Popups and notifications appear similar, but are actually quite different:
    - Model.Popup is a function that takes a STRING and produces a ReactElement.
    - Model.Notifications are a functions that take DISPATCH and produce a
      ReactElement.
    This means that at the moment of creation, a popup must already have the
    dispatch function, while the notification does not. This, in turn, means
    that notifications can be created from messages dispatched by JS code.
*)


(*

Popups must be careful in handling internal state because this cannot be updated by
dispatch as would be expected.

viewpopup model ->
model.Popup model.PopupDialogData -> (PopupDialogData contains memory setup data (widths) but not memory component data)

model.Popup <-- 
    unclosablePopup maybeTitle (body memoryEditorData) maybeFoot extraStyle
        (from showMemoryEditorPopup maybeTitle body maybeFoot extraStyle dispatch)

        Here body contains the relevant state and is generated from:

        let openMemoryEditor memory compId model dispatch : unit =
        ....
        let body = makeEditor memory compId model dispatch
        ....
        showMemoryEditorPopup (Some title) body (Some foot) popupExtraStyle dispatch


Although showPopup

*)

module PopupView

open Fulma
open Fable.React
open Fable.React.Props

open JSHelpers
open Helpers
open ModelType
open CommonTypes
open DiagramStyle


//=======//
//HELPERS//
//=======//

let openInBrowser url =
    (fun _ -> Electron.Electron.electron.shell.openExternal url |> ignore)

let extractLabelBase (text:string) : string =
    text.ToUpper()
    |> Seq.takeWhile (fun ch -> ch <> '(')
    |> Seq.filter (fun ch -> System.Char.IsLetterOrDigit ch || ch = '_')
    |> Seq.map (fun ch -> ch.ToString())
    |> String.concat ""

let formatLabelAsBus (width:int) (text:string) =
    let text' = extractLabelBase text
    match width with
    | 1 -> text'
    | _ -> sprintf "%s(%d:%d)" text' (width-1) 0
   

let formatLabelFromType compType (text:string) =
    let text' = extractLabelBase text
    match compType with
    | Input 1 | Output 1 -> text'
    | Input width | Output width -> sprintf "%s(%d:%d)" text' (width-1) 0
    | _ -> text'


let formatLabel (comp:Component) (text:string) =
    formatLabelFromType comp.Type (text:string)

let setComponentLabel model (comp:Component) text =
    let label = formatLabel comp text
    Sheet.editComponentLabel comp.Id label model.Diagram

let setComponentLabelFromText model (comp:Component) text =
    Sheet.editComponentLabel comp.Id text model.Diagram

//========//
// Popups //
//========//

let getText (dialogData : PopupDialogData) =
    Option.defaultValue "" dialogData.Text

let getInt (dialogData : PopupDialogData) =
    Option.defaultValue 1 dialogData.Int

let getInt2 (dialogData : PopupDialogData) =
    Option.defaultValue 0 dialogData.Int2

let getMemorySetup (dialogData : PopupDialogData) =
    Option.defaultValue (1,1) dialogData.MemorySetup

let getMemoryEditor (dialogData : PopupDialogData) =
    Option.defaultValue
        { Address = None; OnlyDiff = false; NumberBase = Hex ; Start = 0L}
        dialogData.MemoryEditorData

/// Unclosable popup.
let unclosablePopup maybeTitle body maybeFoot extraStyle =
    let head =
        match maybeTitle with
        | None -> div [] []
        | Some title -> Modal.Card.head [] [ Modal.Card.title [] [ str title ] ]
    let foot =
        match maybeFoot with
        | None -> div [] []
        | Some foot -> Modal.Card.foot [] [ foot ]
    Modal.modal [ Modal.IsActive true ] [
        Modal.background [] []
        Modal.Card.card [Props [Style extraStyle]] [
            head
            Modal.Card.body [] [ body ]
            foot
        ]
    ]

let showMemoryEditorPopup maybeTitle body maybeFoot extraStyle dispatch =
    fun dialogData ->
        let memoryEditorData = getMemoryEditor dialogData
        unclosablePopup maybeTitle (body memoryEditorData) maybeFoot extraStyle
    |> ShowPopup |> dispatch

let private buildPopup title body foot close extraStyle =
    fun (dialogData : PopupDialogData) ->
        Modal.modal [ Modal.IsActive true; Modal.CustomClass "modal1"] [
            Modal.background [ Props [ OnClick close ]] []
            Modal.Card.card [ Props [Style extraStyle] ] [
                Modal.Card.head [] [
                    Modal.Card.title [] [ str title ]
                    Delete.delete [ Delete.OnClick close ] []
                ]
                Modal.Card.body [] [ body dialogData ]
                Modal.Card.foot [] [ foot dialogData ]
            ]
        ]



let showWaveSetupPopup maybeTitle (popupBody: MoreWaveSetup option ->ReactElement) maybeFoot extraStyle dispatch =
    fun (dialogData:PopupDialogData)->
        printfn "starting morewavesetup popup function"
        unclosablePopup maybeTitle (popupBody dialogData.WaveSetup) maybeFoot extraStyle
    |> ShowPopup |> dispatch


/// Body and foot are functions that take a string of text and produce a
/// reactElement. The meaning of the input string to those functions is the
/// content of PopupDialogText (i.e. in a dialog popup, the string is the
/// current value of the input box.).
let private dynamicClosablePopup title body foot extraStyle dispatch =
    buildPopup title body foot (fun _ -> dispatch ClosePopup) extraStyle
    |> ShowPopup |> dispatch

/// Create a popup and add it to the page. Body and foot are static content.
/// Can be closed by the ClosePopup message.
let closablePopup title body foot extraStyle dispatch =
    dynamicClosablePopup title (fun _ -> body) (fun _ -> foot) extraStyle dispatch

/// Create the body of a dialog Popup with only text.
let dialogPopupBodyOnlyText before placeholder dispatch =
    fun (dialogData : PopupDialogData) ->
        div [] [
            before dialogData
            Input.text [
                Input.Props [AutoFocus true; SpellCheck false]
                Input.Placeholder placeholder
                Input.OnChange (getTextEventValue >> Some >> SetPopupDialogText >> dispatch)
            ]
        ]

/// Create the body of a dialog Popup with only an int.
let dialogPopupBodyOnlyInt beforeInt intDefault dispatch =
    intDefault |> Some |> SetPopupDialogInt |> dispatch
    fun (dialogData : PopupDialogData) ->
        div [] [
            beforeInt dialogData
            br []
            Input.number [
                Input.Props [Style [Width "60px"]; AutoFocus true]
                Input.DefaultValue <| sprintf "%d" intDefault
                Input.OnChange (getIntEventValue >> Some >> SetPopupDialogInt >> dispatch)
            ]
        ]
/// Create the body of a dialog Popup with two ints.
let dialogPopupBodyTwoInts (beforeInt1,beforeInt2) (intDefault1,intDefault2) (width2:string) dispatch =

    let setPopupTwoInts (whichInt:IntMode) =
        fun n -> (Some n, whichInt) |> SetPopupDialogTwoInts |> dispatch

    setPopupTwoInts FirstInt intDefault1 
    setPopupTwoInts SecondInt intDefault2 

    fun (dialogData : PopupDialogData) ->
        div [] [
            beforeInt1 dialogData
            br []
            Input.number [
                Input.Props [Style [Width "60px"]; AutoFocus true]
                Input.DefaultValue <| sprintf "%d" intDefault1
                Input.OnChange (getIntEventValue >> setPopupTwoInts FirstInt)
            ]
            br []
            beforeInt2 dialogData
            br []
            Input.number [
                Input.Props [Style [Width width2]; AutoFocus true]
                Input.DefaultValue <| sprintf "%d" intDefault2
                Input.OnChange (getIntEventValue >> setPopupTwoInts SecondInt)
            ]
        ]

/// Create the body of a dialog Popup with both text and int.
let dialogPopupBodyTextAndInt beforeText placeholder beforeInt intDefault dispatch =
    intDefault |> Some |> SetPopupDialogInt |> dispatch
    fun (dialogData : PopupDialogData) ->
        div [] [
            beforeText dialogData
            Input.text [
                Input.Props [AutoFocus true; SpellCheck false]
                Input.Placeholder placeholder
                Input.OnChange (getTextEventValue >> Some >> SetPopupDialogText >> dispatch)
            ]
            br []
            br []
            beforeInt dialogData
            br []
            Input.number [
                Input.Props [Style [Width "60px"]]
                Input.DefaultValue <| sprintf "%d" intDefault
                Input.OnChange (getIntEventValue >> Some >> SetPopupDialogInt >> dispatch)
            ]
        ]

/// Create the body of a memory dialog popup: asks for AddressWidth and
/// WordWidth, two integers.
let dialogPopupBodyMemorySetup intDefault dispatch =
    Some (4, intDefault) 
    |> SetPopupDialogMemorySetup |> dispatch
    fun (dialogData : PopupDialogData) ->
        let addressWidth, wordWidth = getMemorySetup dialogData
        div [] [
            str "How many bits should be used to address the data in memory?"
            br []
            str <| sprintf "%d bits yield %d memory locations." addressWidth (pow2int64 addressWidth)
            br []
            Input.number [
                Input.Props [Style [Width "60px"] ; AutoFocus true]
                Input.DefaultValue (sprintf "%d" 4)
                Input.OnChange (getIntEventValue >> fun newAddrWidth ->
                    Some (newAddrWidth, wordWidth) 
                    |> SetPopupDialogMemorySetup |> dispatch
                )
            ]
            br []
            br []
            str "How many bits should each memory word contain?"
            br []
            Input.number [
                Input.Props [Style [Width "60px"]]
                Input.DefaultValue (sprintf "%d" intDefault)
                Input.OnChange (getIntEventValue >> fun newWordWidth ->
                    Some (addressWidth, newWordWidth) 
                    |> SetPopupDialogMemorySetup |> dispatch
                )
            ]
            br []
            br []
            str "You will be able to set the content of the memory from the Component Properties menu."
        ]

/// Popup with an input textbox and two buttons.
/// The text is reflected in Model.PopupDialogText.
let dialogPopup title body buttonText buttonAction isDisabled dispatch =
    let foot =
        fun (dialogData : PopupDialogData) ->
            Level.level [ Level.Level.Props [ Style [ Width "100%" ] ] ] [
                Level.left [] []
                Level.right [] [
                    Level.item [] [
                        Button.button [
                            Button.Color IsLight
                            Button.OnClick (fun _ -> dispatch ClosePopup)
                        ] [ str "Cancel" ]
                    ]
                    Level.item [] [
                        Button.button [
                            Button.Disabled (isDisabled dialogData)
                            Button.Color IsPrimary
                            Button.OnClick (fun _ -> buttonAction dialogData)
                        ] [ str buttonText ]
                    ]
                ]
            ]
    dynamicClosablePopup title body foot [] dispatch

/// A static confirmation popup.
let confirmationPopup title body buttonText buttonAction dispatch =
    let foot =
        Level.level [ Level.Level.Props [ Style [ Width "100%" ] ] ] [
            Level.left [] []
            Level.right [] [
                Level.item [] [
                    Button.button [
                        Button.Color IsLight
                        Button.OnClick (fun _ -> dispatch ClosePopup)
                    ] [ str "Cancel" ]
                ]
                Level.item [] [
                    Button.button [
                        Button.Color IsPrimary
                        Button.OnClick buttonAction
                    ] [ str buttonText ]
                ]
            ]
        ]
    closablePopup title body foot [] dispatch

/// A static choice dialog popup.
let choicePopup title body buttonTrueText buttonFalseText buttonAction dispatch =
    let foot =
        Level.level [ Level.Level.Props [ Style [ Width "100%" ] ] ] [
            Level.left [] []
            Level.right [] [
                Level.item [] [
                    Button.button [
                        Button.Color IsLight
                        Button.OnClick (buttonAction false)
                    ] [ str buttonFalseText ]
                ]
                Level.item [] [
                    Button.button [
                        Button.Color IsPrimary
                        Button.OnClick (buttonAction true)
                    ] [ str buttonTrueText ]
                ]
            ]
        ]
    closablePopup title body foot [] dispatch

/// Display popup, if any is present.
let viewPopup model =
    match model.PopupViewFunc with
    | None -> div [] []
    | Some popup -> popup model.PopupDialogData

//===============//
// Notifications //
//===============//

let errorNotification text closeMsg =
    fun dispatch ->
        let close = (fun _ -> dispatch closeMsg)
        Notification.notification [
            Notification.Color IsDanger
            Notification.Props [ notificationStyle ]
        ] [
            Delete.delete [ Delete.OnClick close ] []
            str text
        ]

let errorPropsNotification text = errorNotification text ClosePropertiesNotification
let errorFilesNotification text  = errorNotification text CloseFilesNotification

let warningNotification text closeMsg =
    fun dispatch ->
        let close = (fun _ -> dispatch closeMsg)
        Notification.notification [
            Notification.Color IsWarning
            Notification.Props [ notificationStyle ]
        ] [
            Delete.delete [ Delete.OnClick close ] []
            str text
        ]

let warningPropsNotification text = warningNotification text ClosePropertiesNotification
let warningSimNotification text = warningNotification text CloseSimulationNotification

let viewNotifications model dispatch =
    [ model.Notifications.FromDiagram
      model.Notifications.FromSimulation
      model.Notifications.FromFiles
      model.Notifications.FromMemoryEditor
      model.Notifications.FromProperties ]
    |> List.tryPick id
    |> function
    | Some notification -> notification dispatch
    | None -> div [] []

let viewInfoPopup dispatch =
    let makeH h =
        Text.span [ Modifiers [
            Modifier.TextSize (Screen.Desktop, TextSize.Is6)
            Modifier.TextWeight TextWeight.Bold
        ] ] [str h; br[]]
    let title = "ISSIE: Interactive Schematic Simulator and Integrated Editor"

    let body = div [] [
        makeH "Version"
        str Version.VersionString
        br []; br []
        makeH "Acknowledgments"
        str "ISSIE was created by Marco Selvatici (EIE 3rd year) as his BEng final year project. The waveform viewer was created by Edoardo Santi (EEE 3rd year) during Summer UROP work."
        br []; br []
        makeH "Introduction"
    
        str "Issie designs are made of one or more sheets. Each sheet contains components and Input and Output Connectors. \
        If you have a single sheet that is your complete design. Otherwise any \
        sheet can include the hardware defined another by adding a custom component from My Project in the Catalog. Multiple copies of other sheets can be added." 
        br[]; br[]
        str "The Simulation Tab is used mainly for combinational logic and simple clocked logic: \
        the top 'Waveforms >>' button works with clocked circuits and displays waveforms." 
        br[]; br[];
        str "In Issie all clocked components use the same clock signal. \
        Clk connections are not shown: all clk ports are
        automatically connected together. In the waveforms active clock edges are indicated \
        by verticals line through all the waveforms that separate clock cycles. The clock is not shown."
        br[]  ; br[];  
        button [OnClick <| openInBrowser "https://github.com/tomcl/ISSIE"] [ str "See the Issie Github Repo for more information"]
        br[] ; br[]
        makeH "Keyboard shortcuts"
        str "On Mac use Command instead of Ctrl."
        ul [] [
            li [] [str "Save: Ctrl + S"]
            li [] [str "Copy selected diagram items: Alt + C"]
            li [] [str "Paste diagram items: Alt + V"]
            li [] [str "Undo last diagram action: Alt + Z"]
            li [] [str "Redo last diagram action: Alt + Shift + Z"]
        ]
    ]
    let foot = div [] []
    closablePopup title body foot [Width 800] dispatch

