module BatchView

open System
open System.Windows.Forms.DataVisualization.Charting
open System.Collections.ObjectModel

open DataModel
open Var

let private setDataIsChanged _ = 
    MainwindowViewMode.state.IsDataChanged <- true

[<PropertyChanged.ImplementPropertyChanged>]
type TermoCalcItem = 
    {   Item : CalculateTermo.Item
        mutable Remove : ICommand }
    static member createNew y = 
        let x = { Item = y; Remove = null }        
        subscribePropertyChanged y setDataIsChanged
        subscribePropertyChanged x setDataIsChanged
        x   

[<PropertyChanged.ImplementPropertyChanged>]
type Product =
    {   Product : DataModel.Product
        Unselect : ICommand
        AddCalcTermoPoint : ICommand
        ConvertIsns : ICommand
        mutable SendProgrammerView : ICommand
        mutable Prog : ICommand 
        mutable Select : ICommand 
        mutable Flash : EEPROM.ViewModel
        mutable Current : decimal option
        mutable IsInterrogated : bool 
        mutable InfoExt : Alchemy.ProductInfoExt
        
        TermoCalcItems : ObservableCollection<TermoCalcItem> }
    static member createNew b p = 
        let termoPoints = 
            let x = 
                p.CustomTermo 
                |> List.map TermoCalcItem.createNew
            ObservableCollection<TermoCalcItem>(x)     

        let applyTermoPointsUIEdit() = 
            p.CustomTermo <- 
                termoPoints 
                |> Seq.map( fun x -> x.Item )
                |> Seq.sortBy(fun x  -> x.T)
                |> Seq.toList

        let setupTermoCalcItem(x:TermoCalcItem) =
            subscribePropertyChanged x.Item <| fun prop ->
                applyTermoPointsUIEdit()
            x.Remove <- wpfCommnad1 <| fun () -> 
                termoPoints.Remove x |> ignore  

        Seq.iter setupTermoCalcItem termoPoints
        
        termoPoints.CollectionChanged.Add <| fun e ->
            setDataIsChanged()
            applyTermoPointsUIEdit()
                
            if e.Action = NotifyCollectionChangedAction.Add then
                let newItems = e.NewItems |> Seq.cast
                Seq.iter setupTermoCalcItem newItems
                
        
        {   Flash = EEPROM.ViewModel([||])    
            AddCalcTermoPoint = wpfCommnad1 <| fun () -> 
                termoPoints.Add  
                    {   Item = CalculateTermo.Item.createNew()
                        Remove = null }
            ConvertIsns = wpfCommnad1 <| fun () -> 
                let getp = 
                    let m = termoPoints |> Seq.map(fun x -> x.Item.T, x.Item ) |> Map.ofSeq
                    m.TryFind
                match getp 20m with
                | Some ({ I = Some(i_fon20); K = Some(i_sns20) } as z)  ->
                    let f x y  = convertCurrentToKsnsPercent1 i_sns20 i_fon20 x y |> Some
                    for x in termoPoints do
                        match x.Item.I, x.Item.K with
                        | Some i_fon, Some i_sns -> 
                            x.Item.K <- f i_sns i_fon 
                        | _ -> ()
                | _ -> ()

            Product = p
            Current=None
            IsInterrogated=false 
            InfoExt = Alchemy.ProductInfoExt.create b p
            Unselect = null 
            Select = null
            Prog = null
            SendProgrammerView = null
            TermoCalcItems = termoPoints }
        
        
        

    member x.F2 = x.Product.N=0 
    member x.F3 = if x.Product.N % 8 = 0 then  sprintf "Б%d" ( x.Product.N/8 + 1) else ""
    member x.F4 = x.Product.N % 8 = 0 
    member x.F5 = sprintf "K%d" (x.Product.N+1)
    member x.F6 = x.Product.N<8
    member x.F7 = sprintf "%d.%d" ( x.Product.N/8 + 1) (x.Product.N % 8 + 1)

module CalcTermo = 
    let chart = Chart.createNewChart()
    module Points = 
        let Ifon = new Series( LegendText = "", ChartType = SeriesChartType.Point, MarkerSize=10, MarkerStyle=MarkerStyle.Circle )
        let Ksns = new Series( LegendText = "", ChartType = SeriesChartType.Point, MarkerSize=10, MarkerStyle = MarkerStyle.Circle, YAxisType = AxisType.Secondary )

    let Ifon, Ksns = 
        chart.Series.Add(Points.Ifon)
        chart.Series.Add(Points.Ksns)
        Chart.initializeProductChart chart 

    let show b p  = 
        let buff = Array.create EEPROM_SIZE 0xffuy
        let ifon, kch = Alchemy.getProductTermoSeriesPoints b p
        let m = EEPROM.ViewModel(buff)
        m.Ifon <- ifon
        m.Ksns <- kch
        m.ShowSeries (Ifon.Points, Ksns.Points)

        Points.Ifon.Points.Clear()
        for x,y in ifon do
            Points.Ifon.Points.AddXY(x, y) |> ignore
        
        Points.Ksns.Points.Clear()
        for x,y in kch do
            Points.Ksns.Points.AddXY(x, y) |> ignore

module FlashTermo = 
    let chart = Chart.createNewChart()
    let Ifon, Ksns = Chart.initializeProductChart chart  
    let show (m : EEPROM.ViewModel)  = 
        m.ShowSeries(Ifon.Points, Ksns.Points)

[<PropertyChanged.ImplementPropertyChanged>]
type Party =
    {   Products : Product [] 
        Party : Batch 
        mutable IncludeReportOnlyValids : ICommand
        mutable SelectedItem : Product
        mutable ProductsChecked : Nullable<bool>
        mutable ProductsReportIncluded : Nullable<bool> }   
    
    member x.ProductType = 
        Var.productTypeOfBatch x.Party
    

let createNewPartyViewModel (party : Batch) = 

    let getChecked () = 
        if party.Products |> List.forall( fun x -> x.IsChecked ) then Nullable<bool>(true) else
        if party.Products |> List.forall( fun x -> not x.IsChecked ) then Nullable<bool>(false) else
        Nullable<bool>()

    let getReportIncluded () = 
        let prods = party.Products |> List.filter( fun x -> x.IsChecked)
        if prods |> List.forall( fun x -> x.IsReportIncluded ) then Nullable<bool>(true) else
        if prods |> List.forall( fun x -> not x.IsReportIncluded ) then Nullable<bool>(false) else
        Nullable<bool>()

    let updateProductInfoExt p = 
        p.InfoExt <- Alchemy.ProductInfoExt.create party p.Product

    let x = 
        {   Party  = party
            IncludeReportOnlyValids = null
            // выбранная ЭХЯ партии - инициализируется неиспользуемым объектом, должно быть Products.[0]
            SelectedItem = 
                Product.createNew party party.Products.[0]
            ProductsReportIncluded = getReportIncluded ()
            Products  = party.Products |> List.toArray |> Array.map( fun x -> 
                {   Product.createNew party x 
                        with
                            Flash = EEPROM.ViewModel(x.Flash)                    
                            Unselect = wpfCommnad1 <| fun () ->
                                x.IsChecked <- false
                                x.IsReportIncluded <- false  } ) 
            ProductsChecked = getChecked () } 

    let updateInfoExt _ = 
        x.Products |> Array.iter updateProductInfoExt

    x.IncludeReportOnlyValids <- wpfCommnad1 <| fun _ ->
        for p in x.Products do
            p.Product.IsReportIncluded <- p.InfoExt.IsValid 

    

    subscribePropertyChanged x <| fun e ->
        if e.PropertyName="ProductsChecked" then
            x.ProductsReportIncluded <- getReportIncluded ()
            if not x.ProductsChecked.HasValue then () else
                let v = x.ProductsChecked.Value 
                for x in x.Party.Products do
                    if x.IsChecked <> v then
                        x.IsChecked <- v
        else if e.PropertyName="ProductsReportIncluded" then
            if not x.ProductsReportIncluded.HasValue then () else
                let v = x.ProductsReportIncluded.Value 
                for x in x.Party.Products |> List.filter( fun x -> x.IsChecked) do
                    if x.IsReportIncluded <> v then
                        x.IsReportIncluded <- v        

    subscribePropertyChanged party <| fun e ->
        if e.PropertyName="PGS1" || e.PropertyName="PGS3" then
            updateInfoExt()
    
    x.Products |> Array.iter( fun p -> 

        p.Select <- wpfCommnad1 <| fun () ->
            x.SelectedItem <- p
            MainwindowViewMode.state.State <- MainwindowViewMode.ProductInfo
            CalcTermo.show party p.Product
            FlashTermo.show p.Flash
        
        let updateTermoChart _ = 
            if x.SelectedItem=p then
                CalcTermo.show party p.Product
        
        subscribePropertyChanged p.Product <| fun e ->
            if e.PropertyName="IsCustomTermo" then 
                if  p.Product.IsCustomTermo && p.Product.CustomTermo.IsEmpty then
                        let xs = 
                            p.InfoExt.ProductType.TermoPoints
                            |> List.map(fun x -> CalculateTermo.Item.create1 x.T x.I x.K  )
                        match xs with
                        | [] -> Alchemy.getProductTermoPointsOriginal party p.Product 
                        | x -> x
                        |> List.iter ( TermoCalcItem.createNew >> p.TermoCalcItems.Add )
                updateTermoChart()
            elif e.PropertyName="Flash" then 
                p.Flash <- EEPROM.ViewModel(p.Product.Flash)

        let valsProps = Set.ofList [ "Ifon"; "Isns" ; "I13"; "I24"; "I35"; "I26"; "I17"; "In"; "If_20"; "Is_20"; "If50"; "Is50" ]
        
        subscribePropertyChanged p.Product <| fun e ->
            setDataIsChanged()
            if e.PropertyName="IsChecked" then
                x.ProductsChecked <- getChecked()  
                x.ProductsReportIncluded <- getReportIncluded()

            elif e.PropertyName="IsReportIncluded" then
                x.ProductsReportIncluded <- getReportIncluded()
            elif Set.contains e.PropertyName valsProps then
                updateProductInfoExt p

        for it in p.TermoCalcItems do
            subscribePropertyChanged it.Item updateTermoChart

        p.TermoCalcItems.CollectionChanged.Add <| fun e -> 
            updateTermoChart()
            if e.Action = NotifyCollectionChangedAction.Add then
                let newItems = Seq.cast e.NewItems
                for (it : TermoCalcItem) in newItems do
                    subscribePropertyChanged it.Item updateTermoChart )    

    x.SelectedItem <- x.Products.[0]
    x


module Report = 
    open System.Windows.Documents

    let showSummaryTable (b:Party) (rows:TableRowGroup) = 
        rows.Rows.Clear()
        b.Products 
        |> Array.filter( fun p -> p.Product.IsChecked && p.Product.IsReportIncluded )
        |> Array.iteri( fun n p ->
            let w = UI.Reports.ProductSummaryTableRow()
            let row = w.Row1 
            w.Rows1.Rows.Remove row |> ignore
            w.RunNumber.Text <- sprintf "%d" (n+1)
            rows.Rows.Add row 
            row.DataContext <- p )

    let showPasports (b:Party) isDax (sect:Section) = 
        sect.Blocks.Clear();
        let rowCount = if isDax then 3 else 4
        let cells = ResizeArray<TableCell>()

        b.Products 
        |> Array.filter( fun p -> p.Product.IsChecked && p.Product.IsReportIncluded )
        |> Array.iteri( fun n p ->
            if n % (rowCount*2) = 0 then
                cells.Clear()
                let table = new Table();
                sect.Blocks.Add table
                let rowGroup = TableRowGroup()
                table.RowGroups.Add rowGroup                
                for nrow in 0..rowCount-1 do                    
                    let row = TableRow()
                    rowGroup.Rows.Add row
                    row.Cells.Add(TableCell())
                    row.Cells.Add(TableCell())
                    cells.Add row.Cells.[0]
                    cells.Add row.Cells.[1]

                sect.Blocks.Add(Section(BreakPageBefore = true ))
            let x = new UI.Reports.ReportPasportControl()
            (if isDax then x.SetDax else x.SetSou)()
            let block = x.Doc1.Blocks |> Seq.nth 0
            x.Doc1.Blocks.Clear()
            let n1 = n % (rowCount*2) 
            cells.[n1].Blocks.Add block
            block.DataContext <- p)