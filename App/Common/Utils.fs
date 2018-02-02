[<AutoOpen>]
module Utils

open System
open System.IO
open System.ComponentModel
open System.Text
open System.Collections.Generic
open Microsoft.FSharp.Reflection 

type LogLevel = NLog.LogLevel
   
let log = NLog.LogManager.GetCurrentClassLogger()

let logOfLevel (level : LogLevel) :  (string -> unit) = 
    if level=LogLevel.Info then log.Info
    elif level=LogLevel.Warn then log.Warn
    elif level=LogLevel.Error then log.Error
    elif level=LogLevel.Fatal then log.Fatal
    elif level=LogLevel.Debug then log.Debug
    else log.Trace



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

type ObservableCollection<'a> with
    member x.Clone1(el) =        
        let x1 = ObservableCollection( x )
        x1.Insert(0,el)
        x.CollectionChanged.Add( fun e -> 
            if e.Action = NotifyCollectionChangedAction.Add then
                e.NewItems |> Seq.cast
                |> Seq.iter x1.Add
            elif e.Action = NotifyCollectionChangedAction.Remove then
                e.OldItems 
                |> Seq.cast 
                |> Seq.iter ( x1.Remove >> ignore ) )
        x1

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

module FlowDoc = 
    open System.Windows.Documents
    open System.IO.Packaging
    open System.Windows.Xps.Packaging
    open System.Windows.Xps.Serialization
    let convertToXPS path (doc : FlowDocument) = 
        let width, height = 768., 676.
        let stream = new MemoryStream()
        use package = Package.Open(stream, FileMode.Create, FileAccess.ReadWrite)
        use xpsDoc = new XpsDocument(package, CompressionOption.Maximum)
        use rsm = new XpsSerializationManager(new XpsPackagingPolicy(xpsDoc), false)
        let paginator = (doc |> box :?> IDocumentPaginatorSource).DocumentPaginator
        paginator.PageSize <- new System.Windows.Size(width, height)
        rsm.SaveAsXaml(paginator)
        rsm.Commit()
        stream.Position <- 0L
        File.WriteAllBytes(path, stream.ToArray())

