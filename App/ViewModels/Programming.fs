module Programming

open System
open System.Collections.ObjectModel

open Thread2
open BatchView

type ResultInfo = 
    | Error of string
    | Writen of byte []
    | Verified
    member x.Error1 = 
        match x with
        | Error x -> x
        | _ -> ""

[<PropertyChanged.ImplementPropertyChanged>]
type ProductProgInfo = 
    {   Product : Product
        Info : ResultInfo }
    static member cretaeNewWriten p writen = 
        {   Info = Writen writen
            Product = p  }
    static member cretaeNewFailed error p  = 
        {   Info = Error error
            Product = p  }
    member x.Foreground = 
        match x.Info with
        | Error _ -> LogLevel.Error
        | _ -> LogLevel.Info
        |> foregroundOfLogLevel 
    member x.IsError = 
        match x.Info with
        | Error _ -> true
        | _ -> false
 
[<PropertyChanged.ImplementPropertyChanged>]
type ProgInfo = 
    {   mutable IsOpen : bool
        mutable IsProgressVisible : bool
        mutable Progress : int
        mutable Text : string
        mutable CanClose : bool
        mutable Level : LogLevel
        mutable HasResult : bool

        mutable IsWaitingContinue : bool

        Result : ObservableCollection<ProductProgInfo> }

    member x.Foreground = 
        foregroundOfLogLevel x.Level

    member x.IsError = 
        x.Level>=LogLevel.Error

    member x.Continue = wpfCommnad1 <| fun () ->
        x.IsWaitingContinue <- false

    member x.AddResult r = 
        safe <| fun () -> 
            x.Result.Add r

    member x.ReplaceResult r r1 = 
        safe <| fun () -> 
            x.Result.Remove r |> ignore
            x.Result.Add {r with Info = r1}

    member x.Close = wpfCommnad1 <| fun () ->
        x.IsOpen <- false
        x.CanClose <- false
    member x.Max = EEPROM.addysCount
    member x.Begin() =
        safe x.Result.Clear
        x.Progress <- 0
        x.Text <- ""
        x.Level <- LogLevel.Info
        x.CanClose <- false
        x.IsOpen <- true
        x.IsProgressVisible <- true
        x.HasResult <- false
    member x.End() =
        x.CanClose <- true
        x.IsProgressVisible <- false
        x.Text <- "Выполнено " + (if x.IsError then "c ошибками" else "без ошибок") 
        while x.IsOpen && isKeepRunning() do
            sleep 50
        if not x.IsError then None else
            x.Result 
            |> Seq.filter(fun z -> z.IsError)
            |> Seq.groupBy( fun z -> z.Info.Error1)
            |> showSeq "\n" ( fun (error,zx) ->  
                sprintf "%s - %s" (zx |> showSeq ", " (fun z -> z.Product.F7)) error) 
            |> Some



let progInfo = 
    let x = 
        {   IsOpen  = false
            IsWaitingContinue = false
            IsProgressVisible  = false
            HasResult = false
            Progress  = 0
            Text = ""
            CanClose  = false
            Level = LogLevel.Error
            Result = ObservableCollection<ProductProgInfo>() }
    let upd _ =
       x.Level <- 
        if x.Result |> Seq.exists( fun p -> p.IsError ) then LogLevel.Error else LogLevel.Info
       x.HasResult <- not <| Seq.isEmpty x.Result
    x.Result.CollectionChanged.Add upd
    subscribePropertyChanged Thread2.processInfo <| fun e ->
        if e.PropertyName="IsPerforming" then
            x.IsOpen <- false
    x

[<AutoOpen>]
module private Helpers =

    let showModalMessage level text = 
        progInfo.Text <- text
        progInfo.Level <- level
        progInfo.IsWaitingContinue <- true
        while Thread2.isKeepRunning() && progInfo.IsOpen && progInfo.IsWaitingContinue do
            sleep 50

    let progProduct n p = 
        progInfo.Progress <- 0
        let calculatedFlash = p.InfoExt.CalculatedFlash
        let is64 = p.InfoExt.ProductType.Is64
        let r = 
            EEPROM.addys |> List.fold( fun r (addy1,addy2) ->
                match r with
                | None when isKeepRunning() ->
                    progInfo.Text <- sprintf "Запись ЭХЯ №%s, %d, %d, %x-%x..." p.F7 (n+1) (if is64 then 64 else 16) addy1 addy2 
                    let r = Prog.write n calculatedFlash.[addy1..addy2] addy1 is64
                    progInfo.Progress <- progInfo.Progress + (addy2 - addy1 + 1)
                    r
                | x -> x ) None
            |> function        
                | None -> ProductProgInfo.cretaeNewWriten p calculatedFlash
                | Some error -> 
                    showModalMessage LogLevel.Error ( sprintf "Ошибка прошивки ЭХЯ %s\n%s" p.F7 error )
                    ProductProgInfo.cretaeNewFailed error p
        progInfo.Progress <- 0
        progInfo.AddResult r
        r

    let read n is64   =
        let returnBytes = Array.create DataModel.EEPROM_SIZE 0xFFuy        
        EEPROM.addys |> List.fold( fun r (addy1,addy2) -> 
            if isKeepRunning() |> not then r else
            match r with
            | None when isKeepRunning() ->
                progInfo.Text <- sprintf "Считывание %d, %x-%x..." (n+1) addy1 addy2 
                let r = Prog.read n addy1 (addy2-addy1+1) is64
                progInfo.Progress <- progInfo.Progress + (addy2 - addy1 + 1)
                match r with 
                | Right readedBytes ->
                    for n in addy1..addy2 do
                        returnBytes.[n] <- readedBytes.[n-addy1]
                    None
                | Left x -> Some x
            | x -> x ) None

    let (|ListToArray|) = List.toArray

    let verify (calculatedBytes : byte []) n is64 =
        EEPROM.addys |> List.fold( fun r (addy1,addy2) -> 
            if isKeepRunning() |> not then r else
            match r with
            | None when isKeepRunning() ->
                progInfo.Text <- sprintf "Проверка %d, %x-%x..." (n+1) addy1 addy2 
                let r = Prog.read n addy1 (addy2-addy1+1) is64
                progInfo.Progress <- progInfo.Progress + (addy2 - addy1 + 1)
                match r with 
                | Left x -> Some x
                | Right (ListToArray readedBytes) when readedBytes <> calculatedBytes.[addy1..addy2] ->
                    sprintf 
                        "ошибка верификации %d, %x-%x.\nЗаписано %s\nСчитано %s" 
                        (n+1) addy1 addy2 
                        (SerialPorts.showBytes calculatedBytes.[addy1..addy2])
                        (SerialPorts.showBytes readedBytes)
                    |> log.Debug
                    Some <| sprintf "%x-%x, несовпадение при верификации" addy1 addy2 
                | _ -> None
            | x -> x ) None




let progProducts products = 
    progInfo.Begin()
    products
    |> Seq.groupBy( fun (p : Product) -> p.Product.N / 8 ) 
    |> Seq.iter( fun (n,products) -> 
        if isKeepRunning() |> not then () else
        progInfo.IsOpen <- true
        showModalMessage LogLevel.Warn ( sprintf "Подключите кассету № %d к программатору" (n+1) ) 
        progInfo.IsProgressVisible <- true
        products 
        |> Seq.choose ( fun p -> 
            match progProduct (p.Product.N % 8) p  with
            | {Info = Writen writen} as r -> Some (r,writen)
            | _ -> None ) 
        |> Seq.iter( fun (p,writen) ->
            Thread2.processInfo.MessageDialog.Title <- "Проверка записи"
            let product = p.Product.Product
            let is64 = p.Product.InfoExt.ProductType.Is64
            match verify writen (product.N % 8 ) is64 with
            | Some error -> 
                showModalMessage LogLevel.Error ( sprintf "Ошибка при проверке прошивки ЭХЯ %s\n%s" p.Product.F7 error )
                Error error
            | None   -> 
                product.Flash <- writen
                Verified
            |> progInfo.ReplaceResult p  ) )
    progInfo.End()



let progProduct (product : Product) = 

    
    progInfo.Begin()
    
    match progProduct 0 product with
    | {Info = Writen writen} as p  ->
        
        match verify writen 0 product.InfoExt.ProductType.Is64 with
        | Some x -> Error x
        | None   -> 
            product.Product.Flash <- writen
            Verified
        |> progInfo.ReplaceResult p 
    | _ -> ()
    progInfo.End()
    
    


    
        







