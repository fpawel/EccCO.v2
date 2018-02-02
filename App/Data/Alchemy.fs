module Alchemy  
open System

open DataModel

let log = Microsoft.FSharp.Core.Operators.log


let ifon3 tab = 
    let v = Map.ofList tab
    match v.TryFind 20m, v.TryFind 50m, v.TryFind (-20m) with
    | Some(v20), Some(v50), Some(v_minus20) ->
        let v_minus40 = v_minus20 - 0.5m*(v20-v_minus20)
        let v0 = v20 - 0.5m*(v20-v_minus20)            
        let v40 = (v50-v20)*0.5m+v20            
        let v45 = (v50 - v40)*0.5m + v40
        let v30 = (v40 - v20)*0.5m + v20
        [   -40m,   v_minus40
            -20m,   v_minus20
            0m,     v0
            20m,    v20
            30m,    v30
            40m,    v40
            45m,    v45 
            50m,    v50]
    | _ -> tab
let ifon2 tab = 
    let v = Map.ofList tab
    match v.TryFind 20m, v.TryFind 50m with
    | Some(v20), Some(v50) ->
        let v_minus40 = 0m;
        let v_minus20 = v20*0.2m
        let v0 = v20*0.5m
        let v40 = (v50 - v20)*0.5m + v20
        let v45 = (v50 - v40)*0.5m + v40
        let v30 = (v40 - v20)*0.5m + v20
        [   -40m,   v_minus40
            -20m,   v_minus20
            0m,     v0
            20m,    v20
            30m,    v30
            40m,    v40
            45m,    v45 
            50m,    v50]
    | _ -> tab    

let ksns2 tab = 
    let v = Map.ofList tab
    match v.TryFind 20m, v.TryFind 50m with
    | Some(v20), Some(v50) ->
        let v_minus40 = 30m
        let v_minus20 = 58m
        let v0 = 82m
        let v40 = (v50-v20)*0.5m+v20
        let v30 = (v40-v20)*0.5m+v20
        let v45 = (v50-v40)*0.5m+v40
        [   -40m,   v_minus40
            -20m,   v_minus20
            0m,     v0
            20m,    v20
            30m,    v30
            40m,    v40
            45m,    v45 
            50m,    v50]
    | _ -> tab
let  ksns3 (tab : (decimal * decimal) list ) = 
    let v = Map.ofList tab
    match v.TryFind 20m, v.TryFind 50m, v.TryFind (-20m) with
    | Some(v20), Some(v50), Some(v_minus20) ->            
        if v_minus20>=0.45m*v20 then 
            let v0 = (v20-v_minus20)*0.5m+v_minus20
            let v_minus40 = 2m*v_minus20-v0
            let v40 = (v50-v20)*0.5m+v20
            let v30 = (v40-v20)*0.5m+v20
            let v45 = (v50-v40)*0.5m+v40
            [   -40m,   v_minus40
                -20m,   v_minus20
                0m,     v0
                20m,    v20
                30m,    v30
                40m,    v40
                45m,    v45 
                50m,    v50]
        elif v_minus20>0m then
            let v0 = (v20-v_minus20)*0.5m+v_minus20;
            let v_minus40 = 2m*v_minus20-v0+1.2m*(45m-v_minus20)/( 0.43429m * ( v_minus20 |> float |> log |> decimal ) )                
            let v40 = (v50-v20)*0.5m+v20
            let v30 = (v40-v20)*0.5m+v20
            let v45 = (v50-v40)*0.5m+v40
            [   -40m,   v_minus40
                -20m,   v_minus20
                0m,     v0
                20m,    v20
                30m,    v30
                40m,    v40
                45m,    v45 
                50m,    v50]
        else tab
    | _ -> tab


let ifon = function
    | AppSets.Pt2 -> ifon2
    | _ -> ifon3

let ksns = function
    | AppSets.Pt2 -> ksns2
    | _ -> ksns3

let getCalcTermoMethod b p = 
    match AppSets.sets.CalculateTermoMethod with
    | Some x -> x
    | None -> 
        let prodType = Var.productTypeOfProduct b p
        prodType.CalculateTermoMethod

let getProductTermoPointsOriginal b p  =     
    let calcTermo = getCalcTermoMethod b p
    let ifon = 
        ifon calcTermo (Var.getIfon p)
        |> List.map( fun (t,x) -> t, x * 1000m)         
    let ksns = ksns calcTermo (Var.getKsnsPoints p) 
    let ts = 
        Set.union
            (ifon |> List.map fst |> Set.ofList)
            (ksns |> List.map fst |> Set.ofList)
    let ifon = Map.ofList ifon 
    let ksns = Map.ofList ksns 
    [   for t in ts do
            yield 
                {   CalculateTermo.T = t 
                    CalculateTermo.I =  Map.tryFind t ifon
                    CalculateTermo.K = Map.tryFind t ksns } ] 

let getProductTermoPoints b p  = 
    if p.IsCustomTermo then
        p.CustomTermo 
    else
        getProductTermoPointsOriginal b p

let getProductTermoSeriesPoints b p =
    let ifon,kch = 
        getProductTermoPoints b p
        |> List.sortBy(fun x -> x.T)
        |> List.map( fun x -> (x.T, x.I), (x.T, x.K) )
        |> List.unzip  
    let f = function t, Some x -> Some(t,x) | _ -> None
    List.choose f ifon, List.choose f kch



let FIfon_mkA b p t = 
    let tab = Var.getIfon p |> List.map( fun (t,x) -> t,x)
    if tab.IsEmpty then None else
    piecewiseLinearApproxi (ifon (getCalcTermoMethod b p) tab) t |> Some

let GetFIfon_20 p = 
    let tab = Var.getIfon p |> List.map( fun (t,x) -> t,x)
    if tab.IsEmpty then None else
        piecewiseLinearApproxi (ifon AppSets.Pt2 tab) (-20m) 
        |> Some

let FKsns b p t = 
    let tab = Var.getKsnsPoints p
    if tab.IsEmpty then None else
    piecewiseLinearApproxi (ksns (getCalcTermoMethod b p) tab) t |> Some

type ProductInfoExt = 
    {   Sensitive : decimal option
        Dfon : decimal option
        Dt : decimal option
        Ksns : decimal option
        Ksns50 : decimal option
        Dn : decimal option 
        Batch : Batch 
        Product : Product }

    member x.Uk =
        match x.Dt with
        | Some i -> Some (i * 77m) , Some (i * 52m)
        | _ -> None, None

    member i.Ft =
        let FKsns = FKsns i.Batch i.Product
        let FIfon = FIfon_mkA i.Batch i.Product
        [   -20m; 0m; 20m; 30m ; 50m ] 
        |> List.map( fun t -> t, (FIfon t, FKsns t) )
        |> Map.ofList 

    member i.Dax = 
        let b = i.Batch
        let t = Var.productTypeOfProduct i.Batch i.Product
        let p = i.Product
        match p.Ifon, p.Isns with
        | Some(i11), Some(i32) when i11<>i32 ->
            let d1 = (b.PGS3-b.PGS1)/(i32 - i11)
            let r = 
                [   p.I13, b.PGS1
                    p.I24, b.PGS2 
                    p.I35, b.PGS3
                    p.I26, b.PGS2
                    p.I17, b.PGS1  ]
                |> List.map( function 
                    | (Some i,pgs) -> d1* (i- i11) - (pgs - b.PGS1) |> Some 
                    | _ -> None  )
            let v = 
                match p.I24, p.I26 with 
                | Some(i24), Some(i26) -> d1* (i24- i26) |> Some
                | _ -> None
            r @ [v]
        | _ -> [ for _ in 0..5 -> None ]

    static member create b p = 
        let Ksns = Var.getK20 b p 
        let FKsns = FKsns b p 
        //let FIfon = FIfon_mkA b p
        {   Batch = b
            Product = p
            Sensitive = Var.getSensitive b.PGS1 b.PGS3 p
            Dfon = match p.Ifon, p.I13 with 
                   | Some i1, Some i2 -> i1 - i2 |> abs |> Some
                   | _ -> None
            Dt = match p.Ifon, p.If50 with 
                 | Some(i20), Some(i50) -> Some ( i50-i20 )
                 | _ -> None
            Ksns = Ksns
            Ksns50 = match p.Is50, FKsns 50m with
                     | Some _, Some k -> Some k
                     | _ -> None
            Dn = match p.In, p.Ifon, Ksns with 
                 | Some i_nei, Some i_fon, Some k20 when k20<>0m -> 
                    Some ( ( i_nei - i_fon ) / k20 )   
                 | _ -> None }

    member i.IsValidIfon = 
        let t = Var.productTypeOfProduct i.Batch i.Product
        let p = i.Product
        match t.Ifon_max, p.Ifon with
        | Some i_max, Some i when abs i >= abs i_max -> false
        | _ -> true

    member i.IsValidDi = 
        let t = Var.productTypeOfProduct i.Batch i.Product
        let p = i.Product
        match i.Dfon, t.DeltaIfon_max with 
        | Some d, Some max_d when abs d >= abs max_d -> false                     
        | _ -> true

    member i.IsValidDt = 
        let t = Var.productTypeOfProduct i.Batch i.Product
        let p = i.Product
        match i.Dt, t.Delta_t_min, t.Delta_t_max with
        | Some delta_i_t, Some d_min, _ when abs delta_i_t <= d_min -> false 
        | Some delta_i_t, _, Some d_max when abs delta_i_t >= d_max -> false
        | _ -> true

    member i.IsValidKsns = 
        let t = Var.productTypeOfProduct i.Batch i.Product
        let p = i.Product
        match i.Ksns, t.Ksns_min, t.Ksns_max with
        | Some k, Some k_min,_ when abs k <= abs k_min -> false
        | Some k, _,Some k_max when abs k >= abs k_max -> false
        | _ -> true

    member i.IsValidKsns50 = 
        let t = Var.productTypeOfProduct i.Batch i.Product
        let p = i.Product
        match i.Ksns50, t.Ks40_min, t.Ks40_max with
        | Some k, Some k_min,_ when abs k <= abs k_min -> false
        | Some k, _,Some k_max when abs k >= abs k_max -> false
        | _ -> true

    member i.IsValidDn = 
        let t = Var.productTypeOfProduct i.Batch i.Product
        let p = i.Product
        match i.Dn, t.Delta_nei_max with
        | Some d, Some d_max when abs d >= abs d_max -> false
        | _ -> true 

    member x.IsValid =
        [   x.IsValidDi
            x.IsValidDt
            x.IsValidDt
            x.IsValidIfon
            x.IsValidKsns
            x.IsValidKsns50 ]
        |> List.reduce (&&)

    member i.ProductType = Var.productTypeOfProduct i.Batch i.Product

    member i.CalculatedFlash = 
        let buff = Array.create EEPROM_SIZE 0xffuy
        let m = EEPROM.ViewModel(buff)
        let ifon, kch = getProductTermoSeriesPoints i.Batch i.Product        
        m.Ifon <- ifon
        m.Ksns <- kch
        m.Gas <- i.ProductType.Gas
        m.Units <- i.ProductType.Units
        m.ProductType <- i.ProductType.Name
        m.ScaleMin <- Some 0m
        m.ScaleMax <- Some i.ProductType.Scale
        m.Sens <- i.Sensitive
        m.Serial <- match i.Product.Serial with Some x -> Some (decimal x) | _ -> None
        m.DateTime <- Some DateTime.Now
        buff