[<AutoOpen>]
module FSharpWpf
open System
open System.Windows
open System.Windows.Media
open System.ComponentModel

open NLog
open PropertyChanged

let safe f = 
    if Application.Current=null then () else
    let disp = Application.Current.Dispatcher
    if disp=null then () else
    if disp.CheckAccess() |> not then        
        disp.Invoke( Action(f) ) |> ignore
    else f()

let foregroundOfLogLevel (x:LogLevel) = 
    let y = 
        if x=LogLevel.Error || x=LogLevel.Fatal then Colors.Yellow
        elif x=LogLevel.Warn then Colors.LightCoral
        elif x=LogLevel.Info then Colors.SkyBlue else Colors.LightGray
    SolidColorBrush(y)

let messageBoxFail ttl msg = 
    MessageBox.Show(msg, ttl, MessageBoxButton.OK, MessageBoxImage.Error ) |> ignore

[<AllowNullLiteral>]
type ViewModelBase() =
    let propertyChangedEvent = DelegateEvent<PropertyChangedEventHandler>()
    interface INotifyPropertyChanged with
        [<CLIEvent>]
        member x.PropertyChanged = propertyChangedEvent.Publish
    member x.RaisePropertyChanged propertyName = 
        safe <| fun () ->
            propertyChangedEvent.Trigger([| x; PropertyChangedEventArgs(propertyName) |])

    member x.AddPropertyChangedListener f = 
        (x :> INotifyPropertyChanged).PropertyChanged.Add <| fun e ->
            f e.PropertyName  

[<ImplementPropertyChanged>]
type Info = 
    {   mutable Text : string        
        mutable Descr : string
        mutable Level : LogLevel 
        mutable DateTime : DateTime }
    member x.SetText text = 
        x.Text <- text
        x.DateTime <- DateTime.Now
    member x.IsError = x.Level >= NLog.LogLevel.Error
    member x.Foreground = 
        foregroundOfLogLevel x.Level
    member x.Border = 
        let y = 
            let x = x.Level
            if x=LogLevel.Error || x=LogLevel.Fatal then Colors.Red
            elif x=LogLevel.Warn then Colors.Green
            elif x=LogLevel.Info then Colors.White else Colors.LightGray
        SolidColorBrush(y)
         
    static member empty() = 
            {   DateTime = DateTime.Now 
                Text = ""
                Descr = ""
                Level  = LogLevel.Info }

[<ImplementPropertyChanged>]
type ProcessInfo =
    {   mutable IsReady : bool
        Info : Info }
    member x.IsProcess = not x.IsReady   
    static member empty() = 
        { Info = Info.empty(); IsReady = false }


module Colors = 
    open System.Windows.Media
    let private cnv1 s = ColorConverter.ConvertFromString(s) :?> Color
    let MidleAqua = cnv1 "#FF569CD6"






