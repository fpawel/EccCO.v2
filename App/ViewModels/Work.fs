module Work

open System
open System.Collections.ObjectModel

open Thread2
open BatchView

[<PropertyChanged.ImplementPropertyChanged>]
type TemperatureInfo = 
    {   mutable IsVisible : bool
        mutable Value : decimal option       
        mutable IsError : bool }

let temperature = 
    let m = 
        {   Value  = None
            IsError = false
            IsVisible = false}
    subscribePropertyChanged Thread2.processInfo <| fun e ->
        if e.PropertyName="IsPerforming" && not Thread2.processInfo.IsPerforming then
            m.IsVisible <- false
    m


[<AutoOpen>]
module private Helpers = 
    open PartiesView
    let (<|>) what foo = 
        HierarchicalOperationViewModels.Single( foo, what)
    let (<||>) what l = HierarchicalOperationViewModels.Scenary( l, what)
    let none() = None
    let createNewOp = HierarchicalOperationViewModels.Operation.fromOp run    
    let readProductsCurrents isCanceled f =
        checkedProducts()
        |> List.fold ( fun r p ->       
            if isCanceled() || notKeepRunning() || (not p.Product.IsChecked) then r
            elif hasNotCheckedProduct() then Some "Опрос ЭХЯ - нет выбранных ЭХЯ" else
            match r with 
            | Some x -> Some x
            | None ->
                p.IsInterrogated <- true            
                let x = Stend.readCurrent p.Product.N
                p.IsInterrogated <- false
                match x with
                | Right x -> 
                    p.Current <- Some x
                    f p x
                    None
                | Left _ -> Some "При опросе ЭХЯ произошла ошибка связи" ) None    

//    let rnd = Random()
//    let readProductsCurrents isCanceled f =
//        checkedProducts()
//        |> List.iter ( fun p ->       
//            if isCanceled() |> not then
//                p.IsInterrogated <- true
//                Thread2.sleep 100
//                p.IsInterrogated <- false
//                let x = rnd.NextDouble() |> decimal
//                p.Current <- Some x
//                f p x )
//        None
    let readTemperature() = 
        temperature.IsVisible <- true
        let r = 
            match Stend.readTemperature() with
            | Left e ->
                temperature.Value <- None
                temperature.IsError <- true
                Some e
            | Right t -> 
                temperature.Value <- Some t
                temperature.IsError <- false
                None
        temperature.IsVisible <- true
        r

    

    
    
    
let menu = 
    let isMessageDialogClosed() = not Thread2.processInfo.MessageDialog.IsOpen
    // опрос токов ЭХЯ покуда весит модальное сообщение 
    let read1() = Thread2.attempt { 
        do! readTemperature()
        return readProductsCurrents isMessageDialogClosed ( fun _ _ -> ()) }
    // 
//    let read1() = 
//        PartiesView.checkedProducts()
//        |> List.iter ( fun p ->
//            let isMessageDialogClosed = isMessageDialogClosed()
//            if isMessageDialogClosed || notKeepRunning() || (not p.Product.IsChecked) then () else
//                p.IsInterrogated <- true            
//                Thread2.sleep 100
//                p.IsInterrogated <- false)
//        None
    
    "Снятие" <||> 
        (   [   (fun p v -> p.Product.Ifon <- Some v),  "Фоновый ток", "ПГС1, 20\"C, для снятия фонового тока"
                (fun p v -> p.Product.Isns <- Some v),  "Ток чувствительности", "ПГС3, +20\"C, для снятия тока чувствительности"
                (fun p v -> p.Product.I13 <- Some v),   "Фоновый ток (3) повторный", "ПГС3, 20\"C, для повторного снятия фонового тока (3)" 
                (fun p v -> p.Product.I24 <- Some v),   "Ток (4) при ПГС2", "ПГС2, 20\"C, для снятия тока (4)" 
                (fun p v -> p.Product.I35 <- Some v),   "Ток (5) при ПГС3", "ПГС3, 20\"C, для снятия тока (5)" 
                (fun p v -> p.Product.I26 <- Some v),   "Ток (6) при ПГС2", "ПГС2, 20\"C, для снятия тока (6)" 
                (fun p v -> p.Product.I17 <- Some v),   "Ток (7) при ПГС1", "ПГС1, 20\"C, для снятия тока (7)"
                (fun p v -> p.Product.In <- Some v),    "Неизмеряемый", "неизмеряемый газ, 20\"C" 
                (fun p v -> p.Product.If50 <- Some v),    "Фоновый ток при +50\"С", "ПГС1, +50\"C, для снятия фонового тока при +50\"С" 
                (fun p v -> p.Product.Is50 <- Some v),    "Чувствительность при +50\"С", "ПГС3, +50\"C, для снятия чувствительности при +50\"С" 
                (fun p v -> p.Product.If_20 <- Some v),    "Фоновый ток при -20\"С", "ПГС1, -20\"C, для снятия фонового тока при -20\"С" 
                (fun p v -> p.Product.Is_20 <- Some v),    "Чувствительность при -20\"С", "ПГС3, -20\"C, для снятия чувствительности при -20\"С"  ]
            |> List.map( fun (f,s1,s2) ->    
                s1 <|> fun () -> Thread2.attempt {
                    do! Thread2.showImportantMessageWith s1 ("Подайте " + s2) read1                        
                    do! readProductsCurrents (fun _ -> false) f  } ) )
    |> createNewOp None

let interrogate = 
    let rec interrogate () = 
        if notKeepRunning() then None else            
            match readProductsCurrents (fun _ -> false) ( fun _ _ -> ()) with
            | None -> interrogate()
            | x -> x 
    "Опрос выбранных ЭХЯ" <|> interrogate
    |> createNewOp None 



    

let progParty = 
    let setProgCmd() = 
        for p in PartiesView.curentPartyInfo.Party.Products do
            p.Prog <- 
                let op =
                    sprintf "Прошивка ЭХЯ %s" p.F7 <|> fun() ->
                        Programming.progProduct p
                    |> createNewOp None
                op.Perform
    
    setProgCmd()
    subscribePropertyChanged PartiesView.curentPartyInfo <| fun e ->
        if e.PropertyName="Party" then
            setProgCmd()

    "Прошивка партии" <|> fun() -> attempt{
        do! PartiesView.checkedProducts() |> Programming.progProducts
        PartiesView.saveCurentBatch() }
    |> createNewOp None

let readFlash = 
    "Считывание EEPROM FLASH" <|> fun() ->
        let r = Programmer.read()
        match r with
        | None -> MainwindowViewMode.state.State <- MainwindowViewMode.ProgrFlash
        | _ -> ()
        r
    |> createNewOp None

let writelash = 
    "Запись EEPROM FLASH" <|> Programmer.write
        
    |> createNewOp None

    

