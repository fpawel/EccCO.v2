namespace EccCO.v2 

module Var =

    open DataModel


    let private mk'code'lst(x : (string*int) list )  =
        let f1 = 
            let x = Map.ofList x
            fun k -> Map.tryFind k x
        let f2 = 
            let x = x |> List.map( fun (x,y) -> y,x) |> Map.ofList 
            fun k -> Map.tryFind k x
        (List.map fst x), f1, f2

    let gases, codeOfGas, gasOfCode = 
        [   "CO", 0x11
            "H₂S", 0x22
            "NH₃", 0x33
            "Cl₂", 0x44
            "SO₂", 0x55
            "NO₂", 0x66
            "O₂", 0x88
            "NO", 0x99
            "HCl", 0xAA ]
        |> mk'code'lst

    let units, codeOfUnits, unitsOfCode  = 
        [   "мг/м3", 2
            "ppm", 3
            "об. дол. %", 7 ]
        |> mk'code'lst



    type ProductType =
        {   mutable Name : string
            mutable Gas : string
            mutable Units : string
            mutable Scale : decimal
            mutable NobleMetalContent : decimal
            mutable LifetimeWarrianty : int 
            mutable Is64 : bool
            mutable CalculateTermoMethod : CalculateTermoMethod 
            mutable Ifon_max : decimal option
            mutable DeltaIfon_max : decimal option
            mutable Ksns_min : decimal option
            mutable Ksns_max : decimal option
            mutable Delta_t_min : decimal option
            mutable Delta_t_max : decimal option
            mutable Ks40_min : decimal option
            mutable Ks40_max : decimal option
            mutable Delta_nei_max : decimal option  
            mutable TermoPoints : CalculateTermo.Item list }

        member x.Name1 = "ИБЯЛ.418425." + x.Name

        member x.Range = 
            sprintf "0-%M" x.Scale

        member x.LifetimeWarrianty1 = x.LifetimeWarrianty + 6

        member x.What = sprintf "%A" x

        member x.CalculateTermoMethod1 
            with get () = CalculateTermoMethod.toString x.CalculateTermoMethod
            and set v = x.CalculateTermoMethod <- CalculateTermoMethod.parse1 v
    
        static member create() = 
            {   Name="035"
                Gas="CO"
                Units="мг/м3"
                Scale=200m
                NobleMetalContent = 0.1626m
                LifetimeWarrianty = 18 
                Is64=false
                CalculateTermoMethod = Pt3
                Ifon_max = None
                DeltaIfon_max = None
                Ksns_min = None
                Ksns_max = None
                Delta_t_min = None
                Delta_t_max = None
                Ks40_min = None
                Ks40_max = None
                Delta_nei_max = None  
                TermoPoints = []}

    let getSensitive pgs1 pgs3 p =
        let d = abs(pgs3 - pgs1)
        match p.Ifon, p.Isns with
        | Some ifon, Some isns when d>0m -> abs(isns-ifon)/d |> Some
        | _ -> None


    let getIfon p = 
        [   -20m,   p.If_20
            20m,    p.Ifon
            50m,    p.If50 ]
        |> List.choose( function t, Some x -> Some(t,x) | _ -> None )

    let getK20 b p  = 
        let dConc = abs (b.PGS3 - b.PGS1)
        if dConc=0m then None else
        match p.Ifon, p.Isns with
        | Some i_fon20, Some i_sns20 when i_fon20 <> i_sns20 -> 
            Some <| (i_sns20-i_fon20)/dConc
        | _ -> None

    let convertCurrentToKsnsPercent1 i_sns20 i_fon20 i_sns i_fon = 
        100m * abs ( (i_sns-i_fon)/(i_sns20 - i_fon20) )

    let convertCurrentToKsnsPercent p  = 
        let none _ _ = None
        match p.Ifon, p.Isns with
        | Some i_fon20, Some i_sns20 when i_fon20 <> i_sns20 -> 
            fun i_sns i_fon -> 
                convertCurrentToKsnsPercent1 i_sns20 i_fon20 i_sns i_fon
                |> Some           
            | _ -> none
        | _ -> none


    let getKsnsPoints p =     
        [  -20m,    p.Is_20,    p.If_20
           20m,    p.Isns,     p.Ifon
           50m,    p.Is50,     p.If50 ]
        |> List.choose( function 
            | t, Some(i1), Some(i2) ->
                match convertCurrentToKsnsPercent p i1 i2 with
                | Some k -> Some( t, k )
                | None -> None
            | _ -> None )




