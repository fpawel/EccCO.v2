module PartiesView

open System
open System.Collections.ObjectModel

open DataModel

type CatalogueT = 
        | Year of int
        | Month of int
        | Day of int
        member x.Name = 
            match x with
            | Year n 
            | Day n -> sprintf "%s%d" (if n<10 then " " else "") n
            | Month n -> monthByNumber n

type ContextT = 
    | Info of  BatchInfo
    | Party of  BatchView.Party
    | Catalogue of CatalogueT
    | Root

let createNewPartyViewModel = BatchView.createNewPartyViewModel >> Party 

module Context = 
    let name = function
        | Catalogue( Year n ) -> sprintf "%d год" n
        | Catalogue( Month n) -> sprintf "%2.0f - %s" ( float n) (monthByNumber n)
        | Catalogue( Day n ) -> sprintf "%2.0f" ( float n)
        | Info y -> y.Name
        | Party y -> y.Party.Name
        | Root -> ""

    let key = function
        | Root -> 0L
        | Info p -> p.Date.Ticks
        | Party p ->  p.Party.Date.Ticks
        | Catalogue( Year n ) -> int64 n
        | Catalogue( Day n) -> int64 n
        | Catalogue( Month n ) -> int64 n 

    let what = function
        | Root -> "корень"
        | Info p -> sprintf "Партия %s %A от %A" p.ProductType p.Name p.Date
        | Party { Party = p } ->  sprintf "Выбранная партия %s %A от %A" p.ProductType p.Name p.Date
        | Catalogue( Year n ) -> sprintf "Каталог %d год" n
        | Catalogue( Day n) -> sprintf "Каталог %A месяц" n
        | Catalogue( Month n ) -> sprintf "Каталог %A" n

    let level = function
        | Root -> 0
        | Catalogue( Year _ ) -> 0
        | Catalogue( Month _ ) -> 1
        | Catalogue( Day _) -> 2
        | Info _ | Party _ ->  3

[<PropertyChanged.ImplementPropertyChanged>]    
type PromptConfirmModel = 
    {   mutable Text : string
        mutable Accept : ICommand }


[<PropertyChanged.ImplementPropertyChanged>]
type CurentPartyInfo  =
    {   mutable Party : BatchView.Party 
        mutable ProductSerialFilter : int option
        mutable ProductTypeFilter : string
        PromptConfirmDelete : PromptConfirmModel }


let curentPartyInfo =     
    {   Party = Batch.createNew() |> BatchView.createNewPartyViewModel 
        ProductSerialFilter = None
        ProductTypeFilter = ""
        PromptConfirmDelete = 
            {   Text  = ""
                Accept = null } }

let setCurrentParty batch =    
    MainwindowViewMode.state.IsDataChanged <- false
    curentPartyInfo.Party <- batch
    
    AppSets.sets.PartyId <- batch.Party.Id    
    for x in (box batch.Party)::( List.map box batch.Party.Products ) do
        subscribePropertyChanged x <| fun _ ->
            MainwindowViewMode.state.IsDataChanged <- true 

let private alert x y = 
    log.Error (sprintf "%s, %s" x y)
    UI.Log.Jouranl.addError x y |> ignore
let private message x y = UI.Log.Jouranl.add x y NLog.LogLevel.Info |> ignore

let private strEqLo (s1:string) (s2:string) =
    s1.Trim().ToLower()=s2.Trim().ToLower()

type Tree(context:ContextT, parent : Tree option) =
    inherit ViewModelBase()

    let items =  ResizeArray<Tree>()
    let items1 =  ObservableCollection<UI.PartyTreeViewModel>()

    let what = 
        match context with
        | Catalogue( Month month ) -> 
            let year = match parent.Value.Context with Catalogue(Year year) -> year | _ -> failwith ""
            sprintf "Каталог %d, %s" year (monthByNumber month)
        | Catalogue( Day day ) -> 
            let year = match parent.Value.parent.Value.Context with Catalogue(Year year) -> year | _ -> failwith ""
            let month = match parent.Value.Context with Catalogue(Month month) -> month | _ -> failwith ""
            sprintf "Каталог %d, %2.0f %s" year (float day) (monthByNumber month) 
        | Catalogue( Year year ) ->  
            sprintf "Каталог %d" year        
        | Info p -> sprintf "Партия %s %A от %A" p.ProductType p.Name p.Date
        | Party {Party=p} ->  sprintf "Выбранная партия %s %A от %A" p.ProductType p.Name p.Date
        | Root -> "корень"

    let ui = 
        UI.PartyTreeViewModel
            (   Items = items1,
                IsSelected = false,
                IsCatalogueItem = ( match context with Catalogue _ -> true | _ -> false ),
                Name = Context.name context,                
                IsBatchInfoItem = (match context with
                                   | Info _ -> true                    
                                   | _ -> false),
                ProductType = (match context with
                               | Catalogue y -> ""
                               | Info y -> y.ProductType
                               | Party {Party=p} -> p.ProductType
                               | Root -> ""   ),
                IsVisible = true,
                What = what,
                Level = Context.level context )
       
    let uiKey (ui:UI.PartyTreeViewModel) = 
        (items |> Seq.find( fun x -> x.UI=ui)).Context |> Context.key
    
    member x.UI = ui
    member __.Context = context
    member private __.items = items
    member private __.items1 = items1
    member private  __.parent : Tree option = parent

    member x.IsSelected  
        with get() = ui.IsSelected
        and set v = 
            if ui.IsSelected<>v then
                ui.IsSelected <- v
                if v then
                    for y in x.getRoot().getAllSubItems() do
                        if y<>x then
                            y.IsSelected <- false

    member x.What = what
        
    member x.BatchInfo = 
        match context with
        | Info info -> info
        | Party {Party=p} -> BatchInfo.creteNew p
        | _ -> failwith "getPartyId - getBatchInfo - bad content"
    
    member x.createNewTree item = 
        let y = Tree(item, Some x)
        items.Add y        
        y.UI.Delete <- y.Delete
        y.UI.Select <- y.Select
        let k = Context.key y.Context
        let rec loop n = 
            if n<items1.Count then 
                let x = uiKey items1.[n]
                if k < x then n else loop (n + 1)
            else n
        let n = loop 0
        items1.Insert(n, y.UI)
        match item with
        | Party p -> 
            setCurrentParty p
            x.getRoot().UpdateHasSelectedParty()
            subscribePropertyChanged p.Party <| fun _ ->                
                y.UI.Name <- p.Party.Name
                y.UI.What <- y.What
                y.UI.ProductType <- p.Party.ProductType
        | _ -> ()
        y   

    member x.removeCurentPartyTree() =
        let currentPartyTree,party = x.getRoot().getCurrentParty()
        let parent = currentPartyTree.parent.Value
        parent.remove currentPartyTree
        parent.createNewTree( Info <| BatchInfo.creteNew party ) |> ignore

    member private x.remove item = 
        items.Remove(item) |> ignore
        items1.Remove(item.UI) |> ignore

    member private x.hasSelectedParty() =
        match context with
        | Info _ -> false
        | Party _ -> true            
        | _ -> items  |> Seq.exists( fun x -> x.hasSelectedParty() )

    member private x.getBatchInfoTree id =  
        x.getRoot().getAllSubItems() |> List.tryFind( fun x ->
            match x.Context with
            | Info b when b.Id = id -> true
            | _ -> false )

    member x.Delete = wpfCommnad1 <| fun () -> 
        match x.getPath() with
        | None ->  failwithf "Нельзя удяляить элемент %A" x.Context
        | Some path -> 
            let m = curentPartyInfo.PromptConfirmDelete
            m.Text <- x.What
            m.Accept <- wpfCommnad1 <| fun () ->
                let parent = parent.Value
                try
                    IO.Directory.Delete( path, true) 
                    x :: (x.getAllSubItems()) |> List.choose( fun y -> 
                        match y.Context with 
                        | Party {Party=p} -> Some p.Id 
                        | Info z -> Some z.Id
                        | _ -> None)
                    |> Set.ofList
                    |> Repository.removeByIds
                    parent.remove x |> ignore                
                    message "Данные удалены" (sprintf "%A" x.What) 
                    match x.getRoot().tryGetCurrentParty() with
                    | None -> 
                        match Repository.getLast() with
                        | Right party -> 
                            match x.getRoot().getBatchInfoTree party.Id with
                            | Some tree -> tree.parent.Value.remove tree
                            | _ -> ()                    
                            x.openParty party
                        | Left _ -> x.createNewParty()
                    | _ -> ()
                with e -> 
                    alert ( sprintf "Ошибка при удалении %A" x.What ) ( sprintf "%A" e)

    member x.Select =
        let canExec() = match context with Info _ -> true | _ -> false 
        wpfCommnad canExec x.openPartyOfBatchInfo 

    member private x.getRoot() : Tree = 
        match parent with        
        | Some x -> x.getRoot()
        | None -> x
            
    member private x.getAllSubItems() : Tree list =  
        [   yield! items
            for x in items do
                yield! x.getAllSubItems() ]

    member private x.getParents() = 
        match parent with
        | None -> []
        | Some parent -> parent :: ( parent.getParents())

    member private x.getPath() = 
        let path = x :: (x.getParents()) |> Seq.map( fun x -> x.Context ) |> Seq.toList
        match path with
        |   (Catalogue (Day day) ):: 
            (Catalogue (Month month))::
            (Catalogue (Year year) ) ::_ -> Repository.Path.day year month day |> Some
        |   (Catalogue (Month month))::
            (Catalogue (Year year) ) ::_ -> Repository.Path.month year month |> Some
        |   (Catalogue (Year year))::_ -> Repository.Path.year year |> Some
        |   (Info y)::_ -> Repository.Path.batch y.Id y.Date |> Some
        |   (Party {Party=p})::_ -> Repository.Path.batch p.Id p.Date |> Some
        |   _  -> None

    member x.getCatalogue catalogue = 
        let y = items |> Seq.tryFind( fun y ->
            match y.Context with 
            | Catalogue (catalogue') when catalogue' = catalogue -> true 
            | _ -> false )
        match y with 
        | Some y -> y
        | _ -> x.createNewTree ( Catalogue catalogue )

    member x.UpdateHasSelectedParty() = 
        x.getAllSubItems() |> List.iter( fun x -> x.UI.HasSelectedParty <- x.hasSelectedParty() )

    member x.openParty (party:Batch) = 
        let date = party.Date
        let root = x.getRoot()
        root.getCatalogue( Year date.Year)
            .getCatalogue( Month date.Month)
            .getCatalogue( Day date.Day )
            .createNewTree ( createNewPartyViewModel party )
        |> ignore

    member x.createNewParty() = 
        Repository.createNew() |> x.openParty

    member x.tryGetCurrentParty() : (Tree * Batch) option = 
        match context with
        | Party y -> Some(x,y.Party)
        | Info _ -> None        
        | _ ->  
            match items |> Seq.choose( fun x -> x.tryGetCurrentParty() ) |> Seq.toList with
            | [x] -> Some x
            | _ -> None

    member x.getCurrentParty() : (Tree * Batch) = 
        match x.tryGetCurrentParty() with
        | Some y -> y        
        | _ ->  failwith "Current party not found"

    member private x.openPartyOfBatchInfo()  = 
        let batchInfo = match context with Info x -> x | _ -> failwith "SetCurrentParty"        
        match Repository.get batchInfo.Id with
        | Left error -> 
            log.Debug error
            alert
                "Ошибка формата открываемых данных"
                (sprintf "Данные партии %A от %A имеют не правильный формат." batchInfo.Name batchInfo.Date) 
        | Right batch ->
            let _ =            
                let treeItem, party = x.getRoot().getCurrentParty()
                let parent = treeItem.parent.Value
                parent.remove treeItem
                if treeItem<> x then
                    parent.createNewTree(Info <| DataModel.BatchInfo.creteNew party) |> ignore
            let parent = parent.Value
            let treeItem = parent.createNewTree (createNewPartyViewModel batch)
            treeItem.IsSelected <- true
            parent.remove x             
            let s = sprintf "Выбрана партия %A от %A" batchInfo.Name batchInfo.Date
            UI.Log.Jouranl.add s s NLog.LogLevel.Info |> ignore 
            
        x.getRoot().UpdateHasSelectedParty() 

    member x.acceptFilter serial productTypeName = 
        x.UI.IsVisible <-
            match x.Context with        
            | Party _ -> true
            | Info b ->                
                (   match serial with
                    | Some serial -> b.ProductsSerials |> List.exists( (=) serial )
                    | _ -> true ) &&
                    (   match productTypeName with
                        | Some productTypeName -> b.ProductType = productTypeName
                        | _ -> true )            
            | _ -> 
                items |> Seq.iter( fun z -> z.acceptFilter serial productTypeName)
                items |> Seq.exists( fun z -> z.UI.IsVisible )


let openRepository() = 
    let openLastBatch batches =
        match Repository.getLast() with
        | Right party ->  batches, party
        | Left x ->     
            alert "Не удалось открыть последнюю сохранённую партию" x
            let party = Repository.createNew()
            (BatchInfo.creteNew party)::batches, party

    let batches, party = 
        match Repository.getInfoList() with
        | [] -> 
            let batch = Repository.createNew()
            [BatchInfo.creteNew batch], batch
        | batches ->
            match batches |> List.tryFind(fun x -> x.Id = AppSets.sets.PartyId) with
            | None ->
                log.Warn (sprintf "партия %A отсутсвует в репозитории"  AppSets.sets.PartyId )
                openLastBatch batches
            | Some batchInfo ->
                match Repository.get batchInfo.Id with
                | Right party -> batches, party
                | Left x -> 
                    alert (sprintf "Не удалось открыть партию %A %s." batchInfo.Date batchInfo.Name ) x
                    openLastBatch batches
    assert( not batches.IsEmpty )
    AppSets.sets.PartyId <- party.Id    
    batches, party
        

let parties = 
    let root = Tree( Root, None )
    
    let createBatchLeaf (dayCatalogue : Tree) batchinfo =  
        if batchinfo.Id <> AppSets.sets.PartyId then Info batchinfo else
            match Repository.get batchinfo.Id with
            | Left x -> 
                alert (sprintf "Не удалось открыть партию %A %s" batchinfo.Date batchinfo.Name ) x
                Info batchinfo
            | Right party ->  
                createNewPartyViewModel party
        |> dayCatalogue.createNewTree

    let batches, party = openRepository ()

    // построение дерева партий 
    batches 
    |> Seq.groupBy( fun x -> x.Date.Year )
    |> Seq.map( fun ( year, xs) -> 
        let x = root.createNewTree( Catalogue <| Year year)
        x, xs) 
    |> Seq.iter( fun (yearCatalogue,parties) -> 
        parties 
        |> Seq.groupBy( fun x -> x.Date.Month )
        |> Seq.map( fun ( month, xs) -> 
            let x = yearCatalogue.createNewTree( Catalogue <| Month month)
            x,xs) 
        |> Seq.iter( fun( monthCatalogue, parties) -> 
            parties
            |> Seq.groupBy( fun x -> x.Date.Day )
            |> Seq.map( fun ( day, xs) -> 
                let x = monthCatalogue.createNewTree( Catalogue <| Day day)
                x,xs) 
            |> Seq.iter( fun (dayCatalogue, parties) -> 
                parties
                |> Seq.iter ( fun party ->
                    let x = createBatchLeaf dayCatalogue party
                    () ))))
    assert( match root.tryGetCurrentParty() with
            | Some (m, party' ) ->
                match m.Context with 
                | Party { Party = party } when party.Id=party'.Id -> true
                |  _-> false
            | _ -> false )
    root.UpdateHasSelectedParty()

    subscribePropertyChanged curentPartyInfo <| fun e ->
        
        if e.PropertyName="ProductSerialFilter" || e.PropertyName="ProductTypeFilter" then
            let t = 
                match Var.productTypes |> Seq.tryFind( fun t -> t.Name=curentPartyInfo.ProductTypeFilter) with
                 | Some t -> Some t.Name
                 | _ -> None
            root.acceptFilter curentPartyInfo.ProductSerialFilter t

    root

let saveCurentBatch() = 
    let party = curentPartyInfo.Party
    Repository.save party.Party
    MainwindowViewMode.state.IsDataChanged <- false

[<AutoOpen>]
module private Helpers1 = 
    let ``50`` = "50\"С"
    let ``-20`` = "-20\"С"

module InputKsns = 
    
    [<PropertyChanged.ImplementPropertyChanged>]
    type M = 
        {   mutable What : string
            mutable Value  : decimal 
            mutable NeedCalcIfon_20 : Nullable<bool>
            mutable CheckboxNeedCalcIfon_20Visible : bool
            }
        member x.NeedCalcIfon_20Value =
            x.NeedCalcIfon_20.HasValue && x.NeedCalcIfon_20.Value
    let m = 
        let x = 
            {   What=``50``
                Value=0m
                NeedCalcIfon_20 = Nullable<bool>(false)
                CheckboxNeedCalcIfon_20Visible = false
                }

        subscribePropertyChanged x <| fun evt -> 
            if evt.PropertyName = "What" then 
                x.CheckboxNeedCalcIfon_20Visible <- x.What = ``-20``
        
        x


let setDataContext (d:Dynamic) = 
    let items =  parties.UI.Items
    d.["RepositoryTree"] <- items
    d.["CurentPartyInfo"] <- curentPartyInfo
    d.["SaveChanges"] <- wpfCommnad1 saveCurentBatch
    d.["CreateNewBatch"] <- wpfCommnad1 <| fun () ->
        parties.removeCurentPartyTree()
        parties.createNewParty()
    d.["MakeCopyBatch"] <- wpfCommnad1 <| fun () ->
        let b = 
            let p = curentPartyInfo.Party.Party
            {p with 
                Id = createNewId() 
                Name = sprintf "Копия партии %A от %A" p.Name p.Date
                Date = DateTime.Now }
        Repository.save b
        parties.removeCurentPartyTree()
        parties.openParty b
        MainwindowViewMode.state.IsDataChanged <- false
        AppSets.sets.PartyId <- b.Id    

    d.["AlchemyKsnsTInput"] <- InputKsns.m
    d.["AlchemyKsnsTemperatures"] <- [``50``; ``-20``]
    d.["AlchemySetKsnsT"] <- wpfCommnad1 <| fun () ->
        let b = curentPartyInfo.Party.Party
        let m = InputKsns.m
        let is50 = m.What = ``50``

        curentPartyInfo.Party.Party.Products
        |> List.filter (fun p -> p.IsChecked)
        |> List.iter( fun p -> 
            if m.What = ``-20`` &&  m.NeedCalcIfon_20Value then
                p.If_20 <- Alchemy.GetFIfon_20 p    
            
            match p.Ifon, p.Isns, if is50 then p.If50 else p.If_20 with
            | Some ifon20, Some isns20, Some ifon ->
                let Isens = Some <| ifon + (isns20 - ifon20) * m.Value / 100m
                if m.What = ``50`` then 
                    p.Is50 <- Isens                     
                else 
                    p.Is_20 <- Isens
            | _ -> ()  )

let products() = curentPartyInfo.Party.Products |> Array.toList
let checkedProducts() = 
    products()
    |> List.filter( fun p -> p.Product.IsChecked )

let hasNotCheckedProduct() = checkedProducts() |> List.isEmpty 
let hasCheckedProduct() = not <| hasNotCheckedProduct()
    

