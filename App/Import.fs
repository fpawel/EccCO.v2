module Import

open System
open System.Collections.ObjectModel

open Var
open DataModel
open Repository

let private cnv<'a when 'a : (new :unit -> 'a) and 'a : struct and 'a :> ValueType> (x : Nullable<'a>) = 
    if x.HasValue then Some x.Value else None

let private cnv1<'a when 'a : (new :unit -> 'a) and 'a : struct and 'a :> ValueType> f (x : Nullable<'a>) = 
    if x.HasValue then Some ( f x.Value ) else None

let private dec (x:decimal) = 
    let y = x.ToString("G29")
    let z = Decimal.Parse y
    z
let private f = cnv1 dec

let ecc
    (   n : int,
        isChecked : bool,
        isReportIncluded : bool,
        productType : string,
        flash : byte [],
        ifon,
        isns,
        i13,
        i24,
        i35,
        i26,
        i17,
        iN,
        if_20,
        is_20,
        if50,
        is50,
        serial : Nullable<int> ) =
    {   Id = createNewId()
        N = n
        ProductType  = productType
        CustomTermo = []
        IsCustomTermo = false
        IsChecked = isChecked
        IsReportIncluded = isReportIncluded
        Flash = flash
        Ifon = f ifon
        Isns = f isns
        I13  = f i13
        I24 = f i24
        I35 = f i35
        I26 = f i26
        I17 = f i17   
        In = f iN
        If_20  = f if_20
        Is_20  = f is_20
        If50  = f if50
        Is50  = f is50
        Serial  = cnv serial    }

let addBatch 
    (   date : DateTime,
        products : Product [],
        productType,
        pgs1,
        pgs2,
        pgs3,
        name) = 
            
    {   Id  = createNewId()
        ProductsSerials  = Batch.getProductsSerials (Array.toList products )            
        Date  = date
        Products = products |> Array.toList
        ProductType = productType
        PGS1 = dec pgs1
        PGS2  = dec pgs2
        PGS3 = dec pgs3
        Name = name  }
    |> save

let addProductType 
    (   name,
        gas,
        units,
        scale,
        nobleMetalContent,
        lifetimeWarrianty,
        is64,
        termoCalcWay,
        ifon_max,
        deltaIfon_max,
        ksns_min,
        ksns_max,
        delta_t_min,
        delta_t_max,
        ks40_min,
        ks40_max,
        delta_nei_max
    ) = 
    {   Name=name
        Gas=gas
        Units=units
        Scale= dec scale
        NobleMetalContent = dec nobleMetalContent
        LifetimeWarrianty = lifetimeWarrianty
        Is64=is64
        CalculateTermoMethod = CalculateTermo.Method.fromIndex1 termoCalcWay  
        Ifon_max = f ifon_max
        DeltaIfon_max = f deltaIfon_max
        Ksns_min = f ksns_min
        Ksns_max = f ksns_max
        Delta_t_min = f delta_t_min
        Delta_t_max = f delta_t_max
        Ks40_min = f ks40_min
        Ks40_max = f ks40_max
        Delta_nei_max = f delta_nei_max  
        TermoPoints = []}
    |> Var.productTypes.Add
    ()

let saveProductTypes() = 
    saveProductTypes()