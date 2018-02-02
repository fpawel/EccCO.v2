module MainwindowViewMode

type Tag =
    | Normal
    | ProductInfo
    | ProductTypesEdit
    | ReportSummaryTable
    | ReportPasport
    | ProgrFlash

[<PropertyChanged.ImplementPropertyChanged>]
type State = 
    {   mutable State : Tag  
        mutable IsDataChanged : bool }

let state = 
    {   State = Normal 
        IsDataChanged = false }

let setDataContext (d : Dynamic) = 
    d.["MainWindowViewState"] <- state
    d.["SetNormalView"] <- wpfCommnad1 <| fun () ->
        state.State <- Normal
    d.["ShowProductTypesEdit"] <- wpfCommnad1 <| fun () ->
        state.State <- ProductTypesEdit
    d.["SetProgFlashMode"] <- wpfCommnad1 <| fun () ->
        state.State <- ProgrFlash
    