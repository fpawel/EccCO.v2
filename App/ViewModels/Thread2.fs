module Thread2

open System
open System.Collections.ObjectModel

open HierarchicalOperationViewModels


type AttemptPerformOperationBuilder(_isKeepRunning) =
    let _notKeepRunning = _isKeepRunning >> not

    let bind1(p,rest) = 
        if _notKeepRunning() then None else 
            match p with 
            | None -> rest()
            | x -> x 
    
    member b.Bind(p, rest) = bind1(p,rest)

    member b.Bind(p, rest) = 
        match p with 
        | Right x -> rest x
        | Left x -> Some x 

    member b.Delay(f : unit -> string option) = f

    member x.Run(f) = f()
    member x.While(cond, f) =
        if _isKeepRunning() && cond() then bind1( f(), fun _ -> x.While(cond, f)) 
        else None

    member x.For(e:seq<_>,f) = 
        let cursor = e.GetEnumerator()
        let body () =  f cursor.Current        
        try
            x.While(cursor.MoveNext, x.Delay(body) )
        with _ ->
            cursor.Dispose()
            reraise()
        
    member b.ReturnFrom (x : string option) = if _notKeepRunning() then None else x
    member b.Return (_) = None

    member b.Combine(f, v ) = 
        bind1(f, v)
        
    member b.Zero() = None

    member x.TryWith(p, handler) = 
        if _notKeepRunning() then None else 
        try p()
        with  exn -> handler exn 

    member m.TryFinally(f, finalizer) = 
        if _notKeepRunning() then None else 
        try f()
        finally finalizer()

[<AutoOpen>]
module private Helpers1 =
    
    let mutable _wasBreaked = false
    let setCanStart() = 
        _wasBreaked <- false        
    let stop() = 
        _wasBreaked <- true 

    let mutable removeWarningHasNotCheckedProduct = fun () -> ()

    let checkHasNotChecked1(x : Operation) = 
        let r = PartiesView.hasNotCheckedProduct()
        if r then
            removeWarningHasNotCheckedProduct <- 
                UI.Log.Jouranl.add (sprintf "Операция %A не может быть выполнена, поскольку не выбрано ни одной ЭХЯ" x.FullName)  x.FullName NLog.LogLevel.Warn
        r


    let mutable removeWarnWasBreaked = fun () -> ()
    let setWarnWasBreaked (x:Operation) = 
        removeWarnWasBreaked()
        removeWarnWasBreaked <- 
            let m = sprintf "Выполнение операции %A было прервано" x.FullName
            UI.Log.Jouranl.add m m LogLevel.Warn

let isKeepRunning() = 
    not _wasBreaked
let notKeepRunning() = 
    _wasBreaked

let attempt = AttemptPerformOperationBuilder(  isKeepRunning )

let sleep t = 
    let t = TimeSpan.FromMilliseconds( float t )
    let start = DateTime.Now
    while DateTime.Now - start < t && isKeepRunning() do
        System.Threading.Thread.Sleep 50


[<PropertyChanged.ImplementPropertyChanged>]
type MessageDialog =
    {   mutable Text : string
        mutable Title : string
        mutable Level : LogLevel
        mutable IsOpen : bool }

    member x.IsError = x.Level >= LogLevel.Error
    member x.IsWarn = x.Level = LogLevel.Warn
    member x.IsInfo = not x.IsError && not x.IsWarn
    
    member x.Close = wpfCommnad1 <| fun () ->
        x.IsOpen <- false

[<PropertyChanged.ImplementPropertyChanged>]
type ProcessInfo = 
    {   mutable Operation  : Operation 
        mutable StartTime : DateTime
        mutable ElepsedTime : TimeSpan
        mutable IsPerforming : bool  
        MessageDialog : MessageDialog }
    member x.reset() = 
        x.IsPerforming <- false
        x.MessageDialog.IsOpen <- false 
    

let processInfo = 
    {   Operation = Operation.single (fun _ -> () ) "" None
        StartTime = DateTime()
        ElepsedTime = TimeSpan()
        IsPerforming = false
        MessageDialog = 
            {   Text = ""
                Title = ""
                Level = LogLevel.Info
                IsOpen = false } }
    
[<AutoOpen>]
module private Helpers = 

    let performOperation (x : Operation) = 
        if notKeepRunning() then None else
        if x.State.Checked=BoolFalse then 
            log.Warn( sprintf "Операция %A не выбрана" x.FullName )
            None  else
        NLog.MappedDiagnosticsContext.Set("currentOperationName", x.FullName)
        processInfo.StartTime <- DateTime.Now
        processInfo.Operation <- x

        let result =             
            if checkHasNotChecked1 x then Some "нет выбранных ЭХЯ" else            
            match x.Action with
            | Some f ->
                try 
                    f()
                with e ->
                    log.Fatal ( sprintf "Выход при выполнеии операции %A с необработанным исключением  %A" x.FullName e )
                    messageBoxFail x.FullName ( sprintf "Произошла ошибка. Приложение будет закрыто.\n\n%A" e.Message)
                    exit 0
            | _ -> None 
        if notKeepRunning() then
            setWarnWasBreaked x

        NLog.MappedDiagnosticsContext.Set("currentOperationName", "-")
        PartiesView.saveCurentBatch()
        result

    let mutable isRunning = false
    
    let mutable removeInfoMessage = fun () -> ()
    let setMessage x y l = 
        removeInfoMessage()
        removeInfoMessage <- 
            UI.Log.Jouranl.add x y l

let showImportantMessageWith =
    let rec loop f = 
        if isKeepRunning() && processInfo.MessageDialog.IsOpen then
            match f() with 
            | None -> loop f
            |  x -> x
        else None

    fun title text f  ->
        let m = processInfo.MessageDialog
        m.Title <- title
        m.Text <- text
        m.Level <- LogLevel.Warn
        m.IsOpen <- true
        loop f

let showModalMessage level title text =
    let m = processInfo.MessageDialog
    m.Title <- title
    m.Text <- text
    m.Level <- level
    m.IsOpen <- true
    while isKeepRunning() && m.IsOpen do
        sleep 50

let run (x : Operation) = 
    
    //if processInfo.IsPerforming && Object.Equals(processInfo.RunList.[0],x) then () else

    assert (not processInfo.IsPerforming)
    assert (not isRunning)
    processInfo.MessageDialog.IsOpen <- false
    removeWarningHasNotCheckedProduct()
    removeWarnWasBreaked()
    Prog.removeErrorMessage()
    Stend.removeErrorMessage()
    setMessage (sprintf "Выполненяется операция %A" x.FullName) x.FullName LogLevel.Info    
    isRunning <- true
    setCanStart()
    let items = x :: x.tree
    let timer = new Timers.Timer(Interval=1000., AutoReset=true, Enabled=false) 
    timer.Elapsed.Add( fun _ -> 
        processInfo.ElepsedTime <- DateTime.Now - processInfo.StartTime ) 
    timer.Start()
    processInfo.IsPerforming <- true
    
    async{        
        let result = attempt{
            for x in items do        
                if isKeepRunning() then
                    
                    do! performOperation x}
        timer.Stop()
        SerialPorts.Port.closeOpenedPorts (fun _ _ -> true)
        isRunning <- false
        processInfo.reset()
        removeInfoMessage()
        removeInfoMessage <-
            let wht = x.FullName.ToLower()
            match result with
            | Some error -> UI.Log.Jouranl.add (sprintf "При выполнении операции %A произошла ошибка:\n%s" wht error) error NLog.LogLevel.Error
            | _ -> UI.Log.Jouranl.add (sprintf "Завершилось выполнение операции %A" wht)  x.FullName NLog.LogLevel.Info  }
    |> Async.Start

let Break = wpfCommnad1 stop


