module EEPROM 

open System
open System.Text
open System.Windows.Media
open System.Windows.Media.Imaging

type DataPointCollection = System.Windows.Forms.DataVisualization.Charting.DataPointCollection

open Var

let addys = 
    Range.mkrng 
        [   0x0000, 0x0200
            0x0400, 0x0640
            0x0700, 0x0712 
            0x0720, 0x0727 ] 256
let addysCount = Range.count addys

[<AutoOpen>]
module private Helpers = 

    let PRODUCT'TYPE'STR'LEN = 50   

    type Buff = byte []
    type Bytes = byte seq

    type Addys = Set<int>
    let addys1 (addy1,addy2) = 
        Set.ofList [for n=addy1 to addy2 do yield n]
    
    let setWord (buff:Buff) addy (value:uint16) = 
        buff.[addy] <- byte value
        buff.[addy+1] <- byte (value >>> 8)

    let setFloat (buff:Buff) addy value = 
        let x = BCD6.decimalBcd6 value
        for n in 0..3 do buff.[addy+n] <- x.[n]

    let (<<--) (d : Buff) (x : Bytes) = 
        for n,x in x |> Seq.mapi( fun n x -> n,x ) do
            d.[n] <- x

    let bcd addy (xs:Buff)  =
        let get() = BCD6.bcd6float xs.[addy..addy+4]
        let set v = 
            match v with
            | Some v -> setFloat xs addy v
            | _ -> Array.fill xs addy 4 0xffuy
        get,set, addys1 (addy,addy+3)

    let strCode (intToStr : int -> string option) (strToInt :string -> int option) addy (xs:Buff)  =
        let get() = 
            match intToStr <| int xs.[addy]  with
            | None -> ""
            | Some x -> x
        let set v = 
            xs.[addy] <-
                match strToInt v with
                | Some x -> byte x
                | _ -> 0xFFuy
        get,set, addys1 (addy,addy)    

    let double addy (xs:Buff) = 
        let get() = 
            if xs.[0..3] |> Array.forall ( (=) 0xffuy ) then None else
            try 
                let v = BitConverter.ToDouble(xs, addy) 
                if v < float Decimal.MaxValue && v > float Decimal.MinValue then
                    v |> decimal |> Some
                else
                    None
            with e -> 
                None
        let set v =
            match v with
            | Some (v:decimal) ->
                let bytes = v |> float |> BitConverter.GetBytes        
                for i in 0..7 do xs.[addy + i] <- bytes.[i]
            | None -> Array.fill xs addy 8 0xffuy            
        get, set, addys1 (addy,addy+7)

    let getInt16Float addy (bytes : Buff) = 
        let x = BitConverter.ToInt16(bytes, addy)
        let y = decimal x
        y

let gas     = strCode gasOfCode codeOfGas 0x0600
let units   = strCode unitsOfCode codeOfUnits 0x060A

let dateTime (xs:Buff) =
    let get() = 
        let (~%%) x = int xs.[x]
        let year = 2000 + %% 0x070F
        let month = %% 0x070E
        let day = %% 0x070D
        let h = %% 0x0710
        let m = %% 0x0711
        let s = %% 0x0712
        if isValidDateTime (year,month,day,h,m,s)  then
            new DateTime( year, month, day, h, m, s ) |> Some
        else
            None
    let set (v:DateTime option) = 
        match v with
        | None -> Array.fill xs 0x070D 6 0xffuy
        | Some (x:DateTime) ->  
            let (<--) n v = xs.[n] <- byte v                  
            0x070D <-- x.Day
            0x070E <-- x.Month
            0x070F <-- x.Year-2000
            0x0710 <-- x.Hour 
            0x0711 <-- x.Minute 
            0x0712 <-- x.Second  
    get,set, addys1 (0x070D,0x0712)

let serial = bcd 0x0701 
let scaleMin = bcd 0x0602 
let scaleMax = bcd 0x0606 

let sens bytes = 
    let get,set1,d = double 0x0720 bytes
    let _,set2,d1 = bcd 0x0709 bytes
    get, ( fun x -> set1 x; set2 x), Set.union d d1

let productType (bytes : Buff) = 
    let addy = 0x060B
    let get() =         
        let rec loop acc n = 
            if n = PRODUCT'TYPE'STR'LEN then acc else
            let ch = bytes.[addy+n]
            if ch=0uy || ch=0xffuy then acc else loop (ch::acc) (n+1) 
        let x = loop [] 0 
        System.Text.Encoding.UTF8.GetString( x |> List.rev |> List.toArray )
    let set (v: string) = 
        Array.fill bytes addy PRODUCT'TYPE'STR'LEN 0xffuy
        let len = min PRODUCT'TYPE'STR'LEN v.Length
        if len<2 then () else
        let v = (Encoding.ASCII.GetBytes v).[0..len-1]
        for n in 0..len-1 do 
            bytes.[addy+n] <- v.[n]
        bytes.[addy+len] <- 0uy
    get,set, addys1 (0x060B, 0x060B + PRODUCT'TYPE'STR'LEN)

let Ifon bytes = 
    let tAddy = 
        List.append
            ( [0 .. 2 .. 0x00F8] |> List.mapi( fun n x -> -n,x) |> List.rev )
            ( [0x0100 .. 2 .. 0x01F8] |> List.mapi( fun n x -> n,x) )
    let get () = 
        let f (t,addy) = 
            decimal t, getInt16Float addy bytes
        let x = List.map f tAddy 
        x

    let _,setIfon1,_ = bcd 0x0705 bytes

    let set ifon =
        
        tAddy |> List.iter( fun (t,addy) -> 
            let x1,x2 =
                match ifon with
                | [] -> 0xffuy,0xffuy
                |  _ -> 
                    let x1 = t |> decimal |> piecewiseLinearApproxi ifon 
                    let x2 = Decimal.Floor x1
                    let x3 = float x2 
                    let x4 = uint16 x3
                    byte x4, byte (x4 >>> 8)
            bytes.[addy + 0] <- x1
            bytes.[addy + 1] <- x2 )
        if not <| List.isEmpty ifon then
            setIfon1 (20m |> piecewiseLinearApproxi ifon |> Some )

    get,set, addys1(0,0x01F8)

let Ksns bytes = 
    let t'addy = 
        List.append
            ( [0x0400 .. 2 .. 0x04F8] |> List.mapi( fun n x -> -n,x) |> List.rev )
            ( [0x0500 .. 2 .. 0x05F8] |> List.mapi( fun n x -> n,x) )
    let get () = t'addy |> List.map( fun (t,addy) -> decimal t, getInt16Float addy bytes )

    let set ksens =             
        t'addy |> List.iter( fun (t,addy) -> 
            let x1,x2 =
                match ksens with
                | [] -> 0xffuy,0xffuy
                |  _ -> 
                    let x = t |> decimal |> piecewiseLinearApproxi ksens |> Decimal.Ceiling |> float
                    let x = uint16 x
                    byte x, byte (x >>> 8)
            bytes.[addy + 0] <- x1
            bytes.[addy + 1] <- x2 )
    get,set, addys1(0x0400,0x05F8)

//let private cnv1 = List.map( fun (x:decimal,y:decimal) -> float x, float y)


[<PropertyChanged.ImplementPropertyChanged>]
type ByteM = 
    {   N : int
        mutable Value : byte }
    static member create n x = { N = n; Value = x }





type ViewModel(d) as this = 
    inherit ViewModelBase()

    let mbytes = d |> Array.mapi ByteM.create

    let mbytes1 = Array.toList mbytes

    let gas         = gas d,        "Gas"
    let units       = units d,      "Units"
    let dateTime    = dateTime d,   "DateTime"
    let serial      = serial d,     "Serial"
    let scaleMin    = scaleMin d,   "ScaleMin"
    let scaleMax    = scaleMax d,   "ScaleMax"

    let sens        = sens d,       "Sens"
    let productType = 
        productType d,              "ProductType"
    let ifon       = Ifon d,       "Ifon"
    let ksns       = Ksns d,       "Ksns"


    let addyProp = 
        let (~%%) = function (_,_,x),name -> x,name
        
        [   %% gas
            %% units
            %% dateTime
            %% serial
            %% scaleMin
            %% scaleMax
            %% sens
            %% productType
            %% ifon
            %% ksns ]
        |> List.map( fun (xs,name) ->   
            [   for a in xs do
                    yield a,name ] )
        |> List.concat

    let (~%%) = function (x,_,_),_ -> x()
    
    let (<--) = 
        let f v = function (_,x,_),_ -> x v
        fun x v -> 
            if %% x <> v then
                f v x 
                let _,name = x
                this.RaisePropertyChanged name
                addyProp 
                |> List.filter( snd >> ((=) name) ) |> List.map fst
                |> List.iter( fun n -> 
                    if mbytes.[n].Value<>d.[n] then
                        mbytes.[n].Value <- d.[n] )

    do
        for m in mbytes do
            subscribePropertyChanged m <| fun _ ->
                d.[m.N] <- m.Value
                addyProp 
                |> List.filter( fst >> ((=) m.N) ) |> List.map snd
                |> List.iter this.RaisePropertyChanged

    
    
    member x.IsValid = %% dateTime |> Option.isSome

    member x.ShowSeries (seriesIfon , seriesKch ) =
        x.ShowSeriesIfon seriesIfon
        x.ShowSeriesKsns seriesKch

    member __.ShowSeriesIfon (seriesIfon : DataPointCollection) =
        seriesIfon.Clear()
        let xs_ifon = %% ifon
        for x,y in xs_ifon do
            seriesIfon.AddXY(x, y) |> ignore

    member __.ShowSeriesKsns (seriesKch : DataPointCollection) =        
        seriesKch.Clear()
        for x,y in %% ksns do
            seriesKch.AddXY(x, y) |> ignore
        ()

    member x.Bytes1 = mbytes1

    member x.Source = Array.copy d

    member x.Bytes = 
        mbytes
        |> Seq.mapi( fun n x -> n,x )
        |> Seq.groupBy (fun (n,_) -> n / 16)        
        |> Seq.map ( fun (n,x) ->
            intToHex 4 (n*16),
                x |> Seq.map snd |> Seq.toArray )
        |> Seq.toArray

    member x.Gas            with get() = %% gas         and set v = gas <-- v 
    member x.Units          with get() = %% units       and set v =  units <-- v 
    member x.DateTime       with get() = %% dateTime    and set v =  dateTime <-- v 
    member x.Serial         with get() = %% serial      and set v =  serial <-- v 
    member x.ScaleMin       with get() = %% scaleMin    and set v =  scaleMin <-- v 
    member x.ScaleMax       with get() = %% scaleMax    and set v =  scaleMax <-- v 
    member x.Sens           with get() = %% sens        and set v =  sens <-- v 

    member x.ProductType    with get() = %% productType and set v =  productType <-- v 
    member x.Ifon           with get() = %% ifon       and set v =  ifon <-- v 
    member x.Ksns           with get() = %% ksns       and set v =  ksns <-- v 