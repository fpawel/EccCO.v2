namespace UI

module Log = 
    open System
    open System.Collections.ObjectModel
    open NLog
    open NLog.Targets
    open NLog.Config
    open NLog.Layouts
    open PropertyChanged

    module Jouranl = 
        type Message = 
            {   Info : Info
                mutable Remove : Windows.Input.ICommand }

        let journal = ObservableCollection<Message>()
        let private add1 text descr level =
            if level >= LogLevel.Error then 
                match journal |> Seq.tryFind( fun y -> y.Info.Text=text ) with
                | Some y -> journal.Remove y |> ignore
                | _ -> ()
            let x = 
                {   Info =  {   DateTime = DateTime.Now 
                                Text = text
                                Descr = descr
                                Level  = level }
                    Remove = null }
            if journal.Count=0 then journal.Add x else journal.Insert(0,x)
            let remove() = safe <| fun () ->
                journal.Remove x |> ignore
            x.Remove <- wpfCommnad1 remove
            logOfLevel level <| sprintf "%s - %s" text descr
            remove

        let add text descr level =
            let x = ref None 
            safe <| fun() -> x := Some <| add1 text descr level
            (!x).Value

        let addError text descr =
            add text descr LogLevel.Error 
    
    type Target() = 
        inherit TargetWithLayout()
        let info = Info.empty()
        do
            base.Layout <- SimpleLayout("${message}")
            base.Name <- "operationInfo"
        override x.Write( logEvent : LogEventInfo) =
            base.Write(logEvent)
            info.Text <- logEvent.FormattedMessage
            info.Descr <- logEvent.FormattedMessage
            info.Level <- logEvent.Level

        member __.Info = info

    let target = new Target()
    let config = LogManager.Configuration;
    let rule = LoggingRule("*", LogLevel.Info, target);
    do  
        if config<>null then
            config.AddTarget("operationInfo", target) 
            config.LoggingRules.Add(rule)
            LogManager.Configuration <- config
        else 
            eprintfn "%s" "Не удалось сконфигурировать NLog!"

    let setText x = target.Info.Text <- x


    let setInfo1 x =        
        target.Info.SetText x
        target.Info.Descr <- x
        target.Info.Level <- LogLevel.Info

    let setInfo2 x y = 
        target.Info.SetText x
        target.Info.Descr <- y
        target.Info.Level <- LogLevel.Info

    let setError1 x = 
        Jouranl.add x x LogLevel.Error |> ignore
        setInfo1 x
        target.Info.Level <- LogLevel.Error

    let setError2 x y = 
        Jouranl.add x y LogLevel.Error |> ignore
        setInfo2 x y
        target.Info.Level <- LogLevel.Error