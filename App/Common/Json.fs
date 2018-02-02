module Json

open System
open FParsec
open System.Text
open System.Collections.ObjectModel

type Json = 
    | JString of string
    | JDecimal of decimal    
    | JInt of int64
    | JBool   of bool
    | JNull
    | JList   of Json list
    | JObject of Map<string, Json>
    | JClassObject of string * Json

let (|JProperty|_|) key = function    
    | JObject x -> x |> Map.tryFind key
    | _ -> None


let (|JStringInt|_|) = function    
    | JString x -> 
        let b,x = Int32.TryParse x
        if b then Some x else None
    | _ -> None


let (|JStringInt64|_|) = function    
    | JString x -> 
        let b,x = Int64.TryParse x
        if b then Some x else None
    | _ -> None

let (|JInt32|_|) = function    
    | JInt x -> x |> int |> Some
    | JDecimal x when Decimal.Round(x)=x -> x |> Decimal.ToInt32 |> Some
    | _ -> None

let jempty = JObject ( Map.empty )

[<AutoOpen>]
module private ``распознание json из тескста`` = 
    
    open FParsec
    open FParsec.Primitives

    type Parser<'t> = Parser<'t, unit>
    let jnumber  =
        // -?[0-9]+(\.[0-9]*)?([eE][+-]?[0-9]+)?
        let numberFormat =     NumberLiteralOptions.AllowMinusSign
                           ||| NumberLiteralOptions.AllowFraction
                           ||| NumberLiteralOptions.AllowExponent
        numberLiteral numberFormat "число"
        |>> fun nl -> if nl.IsInteger then JInt (int64 nl.String) else JDecimal (decimal nl.String)
    let jnull       = stringReturn "null" JNull
    let jtrue       = stringReturn "true"  (JBool true)
    let jfalse      = stringReturn "false"  (JBool false)
    let str s = pstring s
    let stringLiteral =
        let escape  = 
            anyOf "\"\\/bfnrt" |>> function 
                | 'b' -> "\b" | 'f' -> "\u000C" | 'n' -> "\n" | 'r' -> "\r" | 't' -> "\t" 
                | c   -> string c // every other char is mapped to itself
        let unicodeEscape  =
            /// converts a hex char ([0-9a-fA-F]) to its integer number (0-15)
            let hex2int c = (int c &&& 15) + (int c >>> 6)*9
            str "u" >>. pipe4 hex hex hex hex (fun h3 h2 h1 h0 ->
                (hex2int h3)*4096 + (hex2int h2)*256 + (hex2int h1)*16 + hex2int h0
                |> char |> string )
        let escapedCharSnippet = str "\\" >>. (escape <|> unicodeEscape)
        let normalCharSnippet  = manySatisfy (fun c -> c <> '"' && c <> '\\')
        let literal = stringsSepBy normalCharSnippet escapedCharSnippet
        between (str "\"") (str "\"") literal

    let jstring  = stringLiteral |>> JString

    let ws = spaces

    let jvalue, jvalueRef = createParserForwardedToRef<Json, unit>()
    
    let jClassObject = 
        let normalCharSnippet  = manySatisfy (fun c -> c <> '"' && c <> '\\' && c <> '(' && c<>')' && c<>'\'')

        normalCharSnippet .>>. between (str "(") (str ")") jvalue |>> JClassObject
        
    let listBetweenStrings sOpen sClose pElement f =
        between (str sOpen) (str sClose)
                (ws >>. sepBy (pElement .>> ws) (str "," >>. ws) |>> f)

    let jlist = listBetweenStrings "[" "]" jvalue JList
    let jEmptyList = str "[" >>. ws >>. str "]" |>> ( fun _ -> JList [] )

    let keyValue  = stringLiteral .>>. (ws >>. str ":" >>. ws >>. jvalue)

    let jobject = listBetweenStrings "{" "}" keyValue (Map.ofList >> JObject)

    do jvalueRef := choice [jobject 
                            attempt jEmptyList
                            jlist
                            jstring
                            jnumber
                            jtrue
                            jfalse
                            jnull                            
                            jClassObject ]
    let json = ws >>. jvalue .>> ws .>> eof
    let parse source = 
        match run json source with
        | Success(result, _, _) -> Right result
        | Failure(errorMsg, _, _) -> Left errorMsg

let fromString = parse


let toString = 
    let csb x = StringBuilder(x:string)
    let csb0 () = StringBuilder()
    let stab n = String('\t',n)
    let rec toString n = function        
        | JString s ->  sprintf "\"%s\"" s |> csb
        | JDecimal n -> string n |> csb
        | JInt n -> string n |> csb
        | JBool v -> sprintf "%b" v |> csb
        | JNull -> "null" |> csb
        | JClassObject (x,y) -> 
            csb0().AppendFormat("{0}({1})", x, toString (n+1) y )
        | JList items ->  
            if items.IsEmpty then csb "[]" else
            items
            |> List.fold( fun (acc:StringBuilder) item ->                 
                let acc = if acc.Length=0 then acc else acc.AppendFormat(",\r\n{0}", stab (n+1))
                acc.Append(  toString (n+1) item ) )  (csb0())
            |> fun x -> csb0().AppendFormat( "[{0}\r\n{1}]", x, stab n )
        | JObject items ->            
            items
            |> Map.fold ( fun (acc:StringBuilder) key item ->                
                try
                    let acc = if acc.Length=0 then acc else acc.AppendFormat(",\r\n{0}", stab (n+1))
                    acc.AppendFormat( "\"{0}\":{1}", key, toString (n+1) item ) 
                with e ->
                    failwithf "%A" e
                )  (csb0())
            |> fun x -> 
                csb0().AppendFormat( "{0}\r\n{1}{2}{3}", "{", stab (n+1), x,  "}" )
    fun x -> (toString 0 x).ToString()

let pretyFormat x = 
    match fromString x with
    | Right x -> toString x
    | _ -> "{}"

[<AutoOpen>]
module private ``сериализация json`` = 
    open Microsoft.FSharp.Reflection 

    let isStringMap (tx:Type) = 
        let x = tx.GetGenericArguments()
        tx.IsGenericType && tx.GetGenericTypeDefinition() = typedefof<Map<string,_>> &&                
        x.Length=2 &&
        x.[0]=typeof<string>

    let isJsonSequense (tx:Type) = 
        if not tx.IsGenericType then false else
        let genType = tx.GetGenericTypeDefinition()
        genType = typedefof<list<_>> || 
        genType = typedefof<Set<_>> ||
        genType = typedefof<ObservableCollection<_>> ||
        genType = typedefof<ResizeArray<_>>
        

    let rec serialize (converters : (Type * (obj -> Json)) list) (x : obj)   = 
        let serialize = serialize converters 
        if x=null then JNull else

        match converters |> List.tryFind( fun (t,_) -> t=x.GetType() ) with
        | Some(_,ctr) -> ctr x
        | None -> 
        

        let (==>) x y = x, y |> int64 |> JInt
        let jobj = Map.ofList >> JObject
        match x with
        | :? string as x -> JString x
        | :? decimal as x -> JDecimal x
        | :? float as x -> JDecimal ( decimal x)
        | :? single as x -> JDecimal ( decimal x)
        | :? int8 as x -> JInt ( int64 x)
        | :? int16 as x -> JInt ( int64 x)
        | :? int as x -> JInt ( int64 x)
        | :? int64 as x -> JInt x
        | :? byte as x -> JInt ( int64 x)
        | :? uint16 as x -> JInt ( int64 x)
        | :? uint32 as x -> JInt ( int64 x)
        | :? uint64 as x -> JInt ( int64 x)
        | :? bool as x -> JBool x
        | :? DateTime as x ->  
            [   "Year" ==> x.Year
                "Month" ==> x.Month
                "Day" ==> x.Day
                "Hour" ==> x.Hour
                "Minute" ==> x.Minute
                "Second" ==> x.Second
                "Millisecond" ==> x.Millisecond ] 
            |> jobj
        | :? TimeSpan as x -> 
            
            [   "Days" ==> x.Days
                "Hours" ==> x.Hours
                "Minutes" ==> x.Minutes
                "Seconds" ==> x.Seconds
                "Milliseconds" ==> x.Milliseconds ] 
            |> jobj
        | _ -> 
            let tx = x.GetType()
            if isJsonSequense tx then                
                x :?> System.Collections.IEnumerable |> Seq.cast |> Seq.toList
                |> List.map serialize 
                |> JList

            elif isStringMap tx then
                x :?> System.Collections.IEnumerable |> Seq.cast |> Seq.toList
                |> List.map ( fun y -> 
                    let ty = y.GetType()
                    let keyProp = ty.GetProperty("Key")
                    let kvpKey = string ( keyProp.GetValue(y, null) )
                    let kvpValue = ty.GetProperty("Value").GetValue(y, null)
                    kvpKey, serialize kvpValue )
                |> Map.ofList |> JObject 

            elif tx.IsGenericType && tx.GetGenericTypeDefinition() = typedefof<Map<_,_>> then
                let keyValuePairType = typedefof<System.Tuple<_,_>>.MakeGenericType( tx.GetGenericArguments() )
                x :?> System.Collections.IEnumerable |> Seq.cast |> Seq.toList
                |> List.map ( fun y -> 
                    let ty = y.GetType()
                    let kvpKey = ty.GetProperty("Key").GetValue(y, null)
                    let kvpValue = ty.GetProperty("Value").GetValue(y, null)                    
                    let keyValue = Activator.CreateInstance(keyValuePairType, [|kvpKey; kvpValue|])
                    serialize keyValue )
                |> JList            
            elif tx.IsArray  then                
                [ for y in (x :?> Array) -> y ] 
                |> List.map serialize |> JList
            elif  FSharpType.IsTuple tx then            
                FSharpValue.GetTupleFields x 
                |> Array.toList
                |> List.map serialize
                |> JList

            elif tx.IsGenericType && tx.GetGenericTypeDefinition() = typedefof<option<_>> then
                match tx.GetProperty("Value").GetValue(x, null) with
                | null -> JNull
                | value -> serialize value                    
                    
            elif FSharpType.IsUnion tx then 
                let case, vals =  FSharpValue.GetUnionFields(x, tx) 
                if vals |> Array.isEmpty then
                    JString case.Name
                else
                    [ case.Name, JList [ for y in vals -> serialize y ] ]
                    |> Map.ofList |> JObject
            elif FSharpType.IsRecord tx then             
                FSharpType.GetRecordFields(tx)
                |> Array.toList
                |> List.choose( fun y -> 
                    if y.PropertyType.IsGenericType && y.PropertyType.GetGenericTypeDefinition() = typedefof<option<_>> then
                        match FSharpValue.GetRecordField(x,y) with
                        | null -> None
                        | value ->
                            let value = y.PropertyType.GetProperty("Value").GetValue(value, null)                            
                            Some( y.Name, serialize value)
                    else
                        (y.Name, FSharpValue.GetRecordField(x,y) |> serialize )
                        |> Some  ) 
                |> Map.ofList |> JObject
            else sprintf "Не найдено конструктора Json для %A" tx |> failwith
            
                

    let makeGenericType (baseType : Type) (types : Type list) =  
        if (not baseType.IsGenericTypeDefinition) then
            invalidArg "baseType" "The base type specified was not a generic type definition." 
        baseType.MakeGenericType ( types |> List.toArray )

    let makeListOf itemType (items : obj list) = 
        let listType = 
            makeGenericType 
            <| typedefof<Microsoft.FSharp.Collections.List<_>> 
            <| [ itemType; ] 
        let add =  
            let cons =  listType.GetMethod ("Cons")            
            fun item list ->
                cons.Invoke (null, [| item; list; |])                 
        let list = 
            let empty = listType.GetProperty ("Empty") 
            empty.GetValue (null, [||]) 
        list
        |> List.foldBack add items

    let jsonToObjConverters = 
        let box (x:obj) = box x |> Some
        let changeType (x:obj) (tx:Type) : obj option =  Convert.ChangeType(x, tx) |> Some
        [   yield typeof<string>, function 
                | JString x -> box x 
                | JInt x -> sprintf "%d" x |> box 
                | JDecimal x -> sprintf "%g" x |> box 
                | JNull -> box ""
                | JClassObject (x,y) -> sprintf "%s(%s)" x (toString y) |> box
                | JList _ ->  box "[...]"
                | JObject _ -> box "{...}"
                | JBool x -> sprintf "%b" x |> box
            
            yield typeof<DateTime>, function
                |   JProperty "Year" (JInt year) &  
                    JProperty "Month" (JInt month) &  
                    JProperty "Day" (JInt day) &  
                    JProperty "Hour" (JInt hour) &
                    JProperty "Minute" (JInt minute) & 
                    JProperty "Second" (JInt second) & 
                    JProperty "Millisecond" (JInt millisecond) ->                    
                        DateTime( int year, int month, int day, int hour, int minute, int second, int millisecond) |> box          
                | JString s ->
                    match DateTime.TryParse(s) with
                    | true, x -> box x
                    | _ -> None
                          
                | _ -> None
            
            yield typeof<TimeSpan>, function
                |   JProperty "Days" (JInt days) &  
                    JProperty "Hours" (JInt hours) &
                    JProperty "Minutes" (JInt minutes) & 
                    JProperty "Seconds" (JInt seconds) & 
                    JProperty "Milliseconds" (JInt milliseconds) ->                    
                        TimeSpan(int days, int hours, int minutes, int seconds, int milliseconds) |> box                
                | _ -> None

            yield!  [   typeof<int8> ; typeof<int16>; typeof<int>; typeof<int64>; typeof<byte> 
                        typeof<uint16>; typeof<uint32> ; typeof<uint64> ]
                    |> List.map( fun tx -> tx, function
                        | JDecimal x -> changeType (Math.Round x) tx
                        | JInt x -> changeType x tx
                        | JBool x -> changeType (if x then 1 else 0) tx
                        | JString x -> 
                            let b,x = Int32.TryParse x
                            if b then changeType x tx else None
                        | _ -> None ) 
            yield!  [ typeof<float> ; typeof<single>; typeof<decimal> ]
                    |> List.map( fun tx -> tx, function
                        | JDecimal x -> changeType x tx
                        | JInt x -> changeType x tx
                        | JBool x -> changeType (if x then 1 else 0) tx
                        | JString x -> 
                            let b,x = Decimal.TryParse x
                            if b then changeType x tx else None
                        | _ -> None ) 
            yield typeof<bool>, function  JBool x -> box x | _ -> None ]

    let objlist : obj list -> obj [] = List.rev >> List.toArray
    let jsonToTypedObj = 
        [   (fun (tx:Type) -> tx.IsGenericType && ( tx.GetGenericTypeDefinition() = typedefof<ResizeArray<_>> || 
                                                    tx.GetGenericTypeDefinition() = typedefof<ObservableCollection<_>>) ),
                fun (tx:Type) src cont ->
                    let valueType = tx.GetGenericArguments().[0]                    
                    match src with 
                    | JList jxs  -> 
                        let values =  jxs |> List.map ( cont valueType ) 
                        
                        match values |> List.tryFind isLeft  with
                        | Some (Left x) -> Left x
                        | _ -> 
                            Activator.CreateInstance( tx, values |> List.choose rightSome |> makeListOf valueType )
                            |> Right
                    | _ -> Left "элемент не является изменяемым списком"
        
            (fun (tx:Type) -> tx.IsGenericType && tx.GetGenericTypeDefinition() = typedefof<list<_>>),
                fun (tx:Type) src cont ->
                    let valueType = tx.GetGenericArguments().[0]                    
                    match src with 
                    | JList jxs  -> 
                        let values = 
                            jxs 
                            |> List.map ( cont valueType ) 
                        match values |> List.tryFind isLeft  with
                        | Some (Left x) -> Left x
                        | _ -> 
                            values |> List.choose rightSome |> makeListOf valueType |> Right
                    | _ -> Left "элемент не является списком"

            isStringMap,
                fun tx src cont ->
                    let valuetype = tx.GetGenericArguments().[1]
                    let keyValuePairType = typedefof<System.Tuple<_,_>>.MakeGenericType( tx.GetGenericArguments() )
                    match src with 
                    | JObject jxs  -> 
                        let xs = 
                            jxs 
                            |> Map.toList
                            |> List.choose ( fun ( key,jvalue) -> 
                                match cont valuetype jvalue with
                                | Left _ -> None  
                                | Right x -> Some (box key,x) ) 
                            |> List.map( fun(key,x) ->                                
                                Activator.CreateInstance( keyValuePairType, [|key; x|] ) )
                        Activator.CreateInstance( tx, makeListOf keyValuePairType xs )
                        |> Right                        
                    | _ -> Left "элемент не является отображением"

            (fun tx -> tx.IsGenericType && tx.GetGenericTypeDefinition() = typedefof<Set<_>> ),
                fun tx src cont ->
                    let valueType = tx.GetGenericArguments().[0]
                    match src with 
                    | JList jxs  -> 
                        let values =  jxs |> List.map ( cont valueType ) 
                        
                        match values |> List.tryFind isLeft  with
                        | Some (Left x) -> Left x
                        | _ -> 
                            Activator.CreateInstance
                                ( tx, values |> List.choose rightSome |> makeListOf valueType )
                            |> Right                        
                    | _ -> Left "элемент не является множеством"

            (fun tx -> tx.IsGenericType && tx.GetGenericTypeDefinition() = typedefof<Map<_,_>> ),
                fun tx src cont ->
                    let keyValuePairType = typedefof<System.Tuple<_,_>>.MakeGenericType( tx.GetGenericArguments() )
                    match src with 
                    
                    | JList jxs  -> 
                        Activator.CreateInstance
                            ( tx,   jxs 
                                    |> List.choose ( cont keyValuePairType >> rightSome) 
                                    |> makeListOf keyValuePairType )
                        |> Right
                    | _ -> Left "элемент не является отображением"

            (fun tx -> tx.IsArray), fun tx src cont ->
                let valueType = tx.GetElementType()            
                match src with 
                | JList jxs  -> 
                    let result = Array.CreateInstance( valueType, jxs.Length )
                    jxs                    
                    |> List.choose ( cont valueType >> rightSome) 
                    |> List.iteri( fun n y ->  result.SetValue( y, n ) )
                    box result |> Right
                | _ -> Left "елемент не является массивом"

            (fun tx -> FSharpType.IsTuple tx), fun tx src cont ->
                let tes = FSharpType.GetTupleElements(tx)
                match src with 
                | JList src when src.Length >= tes.Length ->
                    src |> Seq.take tes.Length |> Seq.toArray |> Array.zip tes 
                    |> Array.fold( fun acc (te,j) ->
                        match acc with 
                        | Left _ -> acc                        
                        | Right acc ->                    
                            match cont te j  with
                            | Left x -> Left ( sprintf "Не удалось десериализовать элемент кортежа - " + x)
                            | Right value -> value::acc |> Right ) (Right [])
                | _ -> Left "Отсутствует кортеж"
                |> function
                | Right values ->                    
                    FSharpValue.MakeTuple( values |> objlist, tx) |> Right
                | Left x -> Left x

            (fun tx -> tx.IsGenericType && tx.GetGenericTypeDefinition() = typedefof<option<_>>), fun tx src cont ->
                let cases =  FSharpType.GetUnionCases tx |> Array.toList 
                match src with
                | JNull -> FSharpValue.MakeUnion(cases.[0], [||]) |> Right
                | _ -> 
                    match cont (tx.GetGenericArguments().[0]) src with
                    | Left x -> Left x
                    | Right x -> FSharpValue.MakeUnion(cases.[1], [|x|]) |> Right

            FSharpType.IsUnion, fun tx src cont -> 
                let cases =  FSharpType.GetUnionCases tx |> Array.toList 
                let (|GetCase|_|) x = 
                    match cases |> List.tryFind( fun case -> case.Name=x) with
                    | Some case -> Some(case, case.GetFields() |> Array.toList )
                    | _ -> None
                let (|MapToList|) = Map.toList
                    
                match src with
                | JString ( GetCase (case,fields) ) when fields.IsEmpty  ->
                    FSharpValue.MakeUnion(case, [||]) |> Right
                | JObject ( MapToList [ GetCase (case,fields), JList js] ) when fields.Length = js.Length ->
                    
                    //JList ((JString ( GetCase (case,fields) )) :: js)  when fields.Length = js.Length ->
                    js |> List.zip fields |> List.fold( fun acc (px,j) ->
                        match acc with 
                        | Left _ -> acc                        
                        | Right acc ->
                            match cont px.PropertyType j  with
                            | Right value -> value::acc |> Right 
                            | Left x -> Left x ) (Right []) 
                    |> function
                        | Right values ->
                            FSharpValue.MakeUnion(case, objlist values) |> Right
                        | Left x -> Left x
                | _ ->  sprintf "не удалось десериализовать размеченное объединение %A %A %s" tx src (toString src)  |> Left
                   
            FSharpType.IsRecord, fun tx src cont ->             
                FSharpType.GetRecordFields(tx) |> Array.fold( fun acc y ->
                    match acc with 
                    | Left _ ->  acc
                    | Right (acc : obj list) ->
                        match src with
                        | JObject jproperties ->
                            match Map.tryFind y.Name jproperties with
                            | None ->                                 
                                if y.PropertyType.IsGenericType && y.PropertyType.GetGenericTypeDefinition() = typedefof<option<_>> then
                                    let case =  (FSharpType.GetUnionCases y.PropertyType).[0]
                                    let value = FSharpValue.MakeUnion(case, [||])
                                    value::acc |> Right 
                                elif y.PropertyType.IsGenericType && y.PropertyType.GetGenericTypeDefinition() = typedefof<Map<_,_>> then                
                                    let keyValuePairType = typedefof<System.Tuple<_,_>>.MakeGenericType( y.PropertyType.GetGenericArguments() )
                                    let value = Activator.CreateInstance( y.PropertyType,  makeListOf keyValuePairType [] )
                                    value::acc |> Right 
                                elif y.PropertyType.IsGenericType && y.PropertyType.GetGenericTypeDefinition() = typedefof<list<_>> then                
                                    let valueType = y.PropertyType.GetGenericArguments().[0]
                                    let value = makeListOf valueType []
                                    value::acc |> Right 
                                else
                                    Left( sprintf "нет значения поля %s.%s, %s" tx.Name y.Name (toString src) )
                            | Some x -> 
                                match cont y.PropertyType x  with
                                | Right value -> value::acc |> Right 
                                | Left x -> Left x
                        | _ -> Left "элемент не является записью" ) (Right [])
                    |> function 
                    | Right values ->  FSharpValue.MakeRecord( tx, objlist values) |> Right
                    | Left x -> Left x  ]

    let rec deserialize jsonToObj (tx:Type) (src : Json) : Either<string,obj>  =
        let deserialize = deserialize jsonToObj
        match jsonToObj |> List.tryFind( fun (t:Type,_) -> t=tx) with
        | Some (_,conv) ->
            match conv src with
            | None -> Left( sprintf "Невозможно получить объект типа %A из %A" tx src )
            | Some x -> Right x 
        | _ ->
            match jsonToTypedObj |> List.tryFind( fun (f,_) -> f tx) with
            | None -> Left( sprintf "Не найден конвертор json для типа %A" tx)
            | Some(_, f) -> f tx src deserialize

let fromJson<'T> addJsonToObj src = 
    let jsonToObj = 
        Seq.append jsonToObjConverters addJsonToObj
        |> Seq.distinctBy( fun (x,_) -> x )
        |> Seq.toList
    try 
        match deserialize jsonToObj typeof<'T> src with
        | Right x -> x :?> 'T |> Right
        | Left x -> Left x
    with e ->  Left e.Message

let toJson = serialize 

let fromObj x = toJson [] x |> toString 

let toObj<'T> x = 
    match fromString x with
    | Left x -> Left x
    | Right x -> fromJson<'T> [] x




