[<AutoOpen>]
module Utils

open System
open System.IO
open System.ComponentModel
open System.Text
open System.Collections.Generic
open Microsoft.FSharp.Reflection 

let rec exnRoot (exn:System.Exception) = 
    if exn.InnerException=null then exn else exnRoot exn.InnerException

let exepath = 
    try
        IO.Path.GetDirectoryName(Reflection.Assembly.GetExecutingAssembly().Location)
    with _ -> 
        
        Environment.CurrentDirectory

let tryGetCaseAttribute<'T,'a> (x:'a) = 
    let case,_ = FSharpValue.GetUnionFields(x, x.GetType() )
    case.GetCustomAttributes() |> 
    Seq.tryFind( fun e -> e.GetType()=typeof< 'T > ) |> 
    Option.map( fun atr -> atr :?> 'T )

let unif<'a> (x:'a) = 
    try
        match FSharpValue.GetUnionFields(x, typeof<'a>) with
        | case, _ -> 
            case.Name
    with e ->
        failwithf "Utils unif %A" e

let caseDescr<'T> (x:'T) = 
    match tryGetCaseAttribute<DescriptionAttribute,'T> x with 
    | None -> unif x
    | Some d -> d.Description

type Dynamic = Dictionary<string,obj>



module Option =
    let mapf f f1 =
        fun x ->
            match f x with
            | None -> None
            | Some x -> Some <| f1 x

let createDirectory x = 
    if not <| Directory.Exists x then
        let x = Directory.CreateDirectory x
        assert x.Exists

let getUniqueKey len = 
    let rec loop x = 
        let x = x + Guid.NewGuid().ToString().GetHashCode().ToString("x")
        if x.Length < len then loop x else x
    loop ""

let monthByNumber n = (Globalization.CultureInfo.CurrentCulture.DateTimeFormat.GetMonthName n).ToLower()

let isValidDate (year,month,day) = 
    year >= DateTime.MinValue.Year && year <= DateTime.MaxValue.Year &&
    month > 0 && month < 13 &&
    day > 0 && day <= DateTime.DaysInMonth(year, month)


let isValidTime (h,m,s) = 
    h>(-1) && h < 24 &&
    m>(-1) && m < 60 &&
    s>(-1) && s < 60

let isValidDateTime (year,month,day,h,m,s) = 
    isValidDate (year,month,day) && isValidTime (h,m,s)

open System.Collections.ObjectModel

// кусочно-линейная апроксимация
let piecewiseLinearApproxi (tab:(decimal*decimal) list ) (t:decimal) =
    let rec loop = function
        | (x,y)::_ when t <= x -> y
        | (x,y)::[] -> y
        | (x1,y1)::(x2,y2)::left when t>=x1 && t<=x2 ->
            let b = (y2-y1)/(x2-x1)
            let a = y1-b*x1
            a + b*t
        | _::v2::left -> loop (v2::left)        
        | [] -> failwith "tab is empty"
    loop tab

let piecewiseLinearApproxi1 tab =
    match tab with
    | [] -> fun _ -> None
    | _ -> piecewiseLinearApproxi tab >> Some

let showSeq<'T> delimString conv (collection : 'T seq )  = 
    collection |> Seq.fold( fun acc x ->
        acc + (if acc |> String.IsNullOrEmpty then acc else delimString) + (conv x) ) ""

let intToHex len x = 
    let x = sprintf "%X" x
    let n = String.length x
    (if n < len then String('0', len-n ) else "") + x


let BoolTrue = Nullable<bool>(true)
let BoolFalse = Nullable<bool>(false)
let BoolNone = Nullable<bool>()

[<AutoOpen>]
module Either =

    type Either<'a, 'b> =
        | Left of 'a
        | Right of 'b

    type either<'a, 'b> = Either<'a, 'b> // lower-case alias like option

    let right = function
      | Right x -> x
      | _      -> failwith "right"

    let isLeft = function
      | Left _ -> true
      | _      -> false

    let isRight = function
      | Right _ -> true
      | _      -> false

    let rightSome = function
      | Right x -> Some x
      | _      -> None

    let leftSome = function
      | Left x -> Some x
      | _      -> None
