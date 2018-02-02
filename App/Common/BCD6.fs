module BCD6

[<AutoOpen>]
module private ``объекты модуля`` = 

    let pow (b:decimal) (e:decimal) =
        let rec loop acc = function
                            | e when e < LanguagePrimitives.GenericOne<_> -> acc
                            | e -> loop (b*acc) (e-LanguagePrimitives.GenericOne<_>)
        loop LanguagePrimitives.GenericOne e

    let (|Digits|) (b:byte) = decimal(b >>> 4), decimal(b &&& 0x0Fuy)
    let (|DecNums2|_|) (Digits (b1,b2)) = if b1<10m && b2<10m then Some(b1,b2) else None
    let (|SignComa|) x = 
        let coma = (x &&& 0b111uy ) |> decimal
        let sign = if (x >>> 7) = 0uy then 1m else (-1m)
        //let porog1 = (x &&& 0b01000uy ) <> 0uy
        //let porog2 = (x &&& 0b10000uy ) <> 0uy
        sign, coma

let (|Bcd6Float|_|) (bcd:byte seq) = 
           
    match Seq.toList bcd with 
    |   SignComa(sign,coma)::
        DecNums2(d'100'000,d10'000)::DecNums2(d1000,d100)::DecNums2(d10,d1)::_ ->
        Some( sign*(d'100'000*100000m + d10'000*10000m + d1000*1000m + d100*100m + d10*10m + d1 )/( pow 10m coma ) ) 
    | _ -> None

let bcd6float  = function (Bcd6Float v) -> Some( v ) | _ -> None

let decimalBcd6 (value:decimal) =

    let comapos =
        let v = abs value
        let (>-<) v1 v2 = v>=v1 && v<v2
        if   0m     >-< 1m then 6m
        elif 1m     >-< 10m then 5m
        elif 10m    >-< 100m then 4m
        elif 100m   >-< 1000m then 3m
        elif 1000m  >-< 10000m then 2m
        elif 10000m >-< 100000m then 1m
        else 0m
    let v =         
        let rec cv v = if v<100000m then v*10m |> cv else v
        let vv v = v*1000000m
        let absv = value |> abs
        absv |> ( if absv<1m then vv else cv) |> round |> int
    
    let b8 = ( (if value<0m then 1uy else 0uy ) <<< 3 ) <<< 4
    let b7 = byte comapos
           
    let b6 = byte( v/100000 ) <<< 4
    let v = v % 100000

    let b5 = byte ( v/10000 )
    let v = v % 10000

    let b4 = byte( v/1000 )  <<< 4
    let v = v % 1000

    let b3 = byte( v/100 )
    let v = v % 100

    let b2 = byte( v/10 )  <<< 4
    let v = v % 10

    let b1 = byte(v)

    [| b8+b7; b6+b5; b4+b3; b2+b1 |]