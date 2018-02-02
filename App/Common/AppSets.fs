module AppSets
open System
open PropertyChanged


type CalculateTermoMethod  =
        | Pt2 
        | Pt3
        static member toString = function
            | Pt2 -> "по двум точкам"
            | Pt3 -> "по трём точкам"

        static member parse1 = function
            | "по двум точкам" -> Pt2
            | _ -> Pt3

        static member TryParse = function
            | "по двум точкам" -> Some Pt2
            | "по трём точкам" -> Some Pt3
            | _ -> None
            
        static member fromIndex1 = function
            | 0 -> Pt2
            | _ -> Pt3

[<PropertyChanged.ImplementPropertyChanged>]
type ComPortSets =
    {   mutable PortName : string
        mutable Timeout : int
        mutable Delay : int
        mutable Chartime : int
        mutable RepeatCount : int
        mutable CanLog : bool 
        mutable BaudRate : int 
        mutable Addy : byte
        Description : string }  

    static member dummy() =
            {   PortName = ""
                Timeout = 1000
                Delay = 0
                Chartime = 20
                RepeatCount = 0

                CanLog = false 
                BaudRate = 9600 
                Addy = 1uy
                Description = "-" }

[<ImplementPropertyChanged>]
type ApplicatioSettings = 
    {   mutable ComportECC : ComPortSets
        mutable ComportProg : ComPortSets
        mutable PartyId : string
        mutable HiddenDataGridProductsColumns : Set<int>  
        mutable CalculateTermoMethod : CalculateTermoMethod option}
    member x.CalculateTermoMethodUI
        with get () = 
            match x.CalculateTermoMethod with 
            | None -> "согласно исполнению"
            | Some a -> CalculateTermoMethod.toString a
        and set value = 
            if value <> x.CalculateTermoMethodUI then 
                x.CalculateTermoMethod <- CalculateTermoMethod.TryParse value

    member x.CalculateTermoMethodUIValues =
        ["согласно исполнению"; "по двум точкам"; "по трём точкам"]


let sets, save = 
    TextConfig.jsonFile "app" <| fun () -> 
        {   PartyId = ""  
            CalculateTermoMethod = None
            HiddenDataGridProductsColumns = 
                [19..25]
                |> Set.ofList
            ComportECC = 
                { ComPortSets.dummy() with Description = "стенд ЭХЯ"}
            ComportProg = 
                { ComPortSets.dummy() with Addy = 0x20uy 
                                           Timeout = 3000
                                           BaudRate = 115200 
                                           Description = "программатор"}}