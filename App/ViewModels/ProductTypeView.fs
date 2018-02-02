module ProductTypeView

open System.Windows.Data
open System.Collections.ObjectModel
open System.ComponentModel


open Var

[<PropertyChanged.ImplementPropertyChanged>]
type Info = 
    {   mutable IsProductTypesChanged : bool  
        mutable FilterGas : string
    }

let info = 
    {   IsProductTypesChanged = false
        FilterGas = "" }

let setDataIsChanged _ = info.IsProductTypesChanged <- true

type TermoPoint = 
    {   Item : DataModel.CalculateTermo.Item
        mutable Remove : ICommand }
    static member createNew y = 
        let x = { Item = y; Remove = null }        
        subscribePropertyChanged y setDataIsChanged
        subscribePropertyChanged x setDataIsChanged
        x 

type ViewModel =    
    {   mutable Remove : ICommand 
        mutable AddTermoPoint : ICommand 
        mutable ProductType : ProductType 
        TermoPoints : ObservableCollection<TermoPoint> }
    static member create (k : ProductType) = 
        let pts =             
            ObservableCollection<TermoPoint> 
                (   k.TermoPoints |> List.map( fun z -> {Item = z; Remove =null} ) )

        let x = 
            {   ProductType = k
                Remove = null
                AddTermoPoint = null
                TermoPoints = pts } 
        let upd _ = k.TermoPoints <- x.TermoPoints |> Seq.map( fun z -> z.Item) |> Seq.toList
        pts |> Seq.iter( fun z -> subscribePropertyChanged z.Item upd )
        pts.CollectionChanged.Add upd

        x.TermoPoints.CollectionChanged.Add( fun e -> 
            if e.Action = NotifyCollectionChangedAction.Add then                
                e.NewItems |> Seq.cast |> Seq.iter ( fun (z : TermoPoint) ->                    
                    z.Remove <- wpfCommnad1 <| fun () ->
                        pts.Remove z |> ignore
                    subscribePropertyChanged z.Item upd ) )
        x.AddTermoPoint <- wpfCommnad1 <| fun () -> 
            x.TermoPoints.Add( { Item = DataModel.CalculateTermo.Item.createNew(); Remove = null} )
        x 
    


[<AutoOpen>]
module private Helpers = 
    let prpCngd x = (box x :?> INotifyPropertyChanged).PropertyChanged
    let types, types1, save, add = 
        let x = ObservableCollection<ViewModel>()
        let save() = 
            if info.IsProductTypesChanged then 
                productTypes.Clear()
                x 
                |> Seq.map( fun x -> x.ProductType )                
                |> Seq.iter( productTypes.Add )
                saveProductTypes()
                info.IsProductTypesChanged <- false
        let setChanged _ = 
            info.IsProductTypesChanged <- true
            ()
        x.CollectionChanged.Add setChanged

        let add = ViewModel.create >> x.Add

        x.CollectionChanged
        |> Observable.filter(fun e -> e.Action = NotifyCollectionChangedAction.Add)
        |> Observable.map(fun e ->  e.NewItems |> Seq.cast )
        |> Observable.add( 
            Seq.iter( fun (y:ViewModel) -> 
                y.Remove <- wpfCommnad1 <| fun () -> 
                    x.Remove y |> ignore 
                (prpCngd y.ProductType ).Add setChanged ) )
           
        productTypes |> Seq.iter add
        info.IsProductTypesChanged <- false

        let v = CollectionViewSource.GetDefaultView(x)
        v.Filter <- fun z ->
            let sfilter = info.FilterGas.Trim()
            if sfilter="" then true else
            let z = z |> box :?> ViewModel
            z.ProductType.Gas = sfilter
        
        subscribePropertyChanged info <| fun e ->
            if e.PropertyName="FilterGas" then
                v.Refresh()

        let x1 =
            {ProductType.create() with Name = "" }
            |> ViewModel.create  
            |> x.Clone1 



        x, x1, save, (wpfCommnad1 <| fun () -> add (ProductType.create()) )

let save = save

let setDataContext (d:Dynamic) = 
    d.["ProductTypes"] <- types
    d.["ProductTypes1"] <- types1
    d.["ProductTypesInfo"] <- info
    d.["SaveProductTypes"] <- wpfCommnad1 save
    d.["AddProductType"] <- add




