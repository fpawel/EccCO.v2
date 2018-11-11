module FSharpBin
open System
//open System.Reflection
open System.IO
open Microsoft.FSharp.Reflection 
open System.Collections.Generic
open System.Collections.ObjectModel

exception BinaryFormatException of string

[<AutoOpen>]
module private ``объекты модуля`` = 

    let isEnumerable (tx:Type) =
        typeof<System.Collections.IEnumerable>.IsAssignableFrom tx || typeof<IEnumerable<_>>.IsAssignableFrom tx

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

let rec serialize (strm:BinaryWriter) (x : obj) : unit  =      
    let serialize = serialize strm
    let tx = x.GetType()
    match x with    
    | :? string as x -> strm.Write x
    | :? decimal as x -> strm.Write(x)
    | :? float as x -> strm.Write x
    | :? single as x -> strm.Write x
    | :? int8 as x -> strm.Write x
    | :? int16 as x -> strm.Write x
    | :? int as x -> strm.Write x
    | :? int64 as x -> strm.Write x
    | :? byte as x -> strm.Write x
    | :? uint16 as x -> strm.Write x
    | :? uint32 as x -> strm.Write x
    | :? uint64 as x -> strm.Write x
    | :? bool as x -> strm.Write x
    | :? DateTime as x -> strm.Write x.Ticks
    | :? (byte []) as bytes ->  strm.Write(bytes.Length)
                                strm.Write(bytes)
    | _ -> 
        if isEnumerable tx then 
            let x = x :?> System.Collections.IEnumerable |> Seq.cast 
            strm.Write "List"
            let len = Seq.length x
            strm.Write len
            if len>0 then                
                x |> Seq.iter serialize
        elif tx.IsGenericType && tx.GetGenericTypeDefinition() = typedefof< Collections.Generic.KeyValuePair<_,_>>  then 
            serialize (tx.GetProperty("Key").GetValue(x, null))
            serialize (tx.GetProperty("Value").GetValue(x, null))
        elif  FSharpType.IsTuple tx then 
            let fs = FSharpValue.GetTupleFields x
            FSharpValue.GetTupleFields x 
            |> Array.toList
            |> List.iter serialize
        elif FSharpType.IsUnion tx then 
            let case, vals =  FSharpValue.GetUnionFields(x, tx) 
            let cases = FSharpType.GetUnionCases tx
            strm.Write(case.Tag)
            vals |> Array.iter serialize
        elif FSharpType.IsRecord tx then             
            FSharpType.GetRecordFields(tx)
            |> Array.iter( fun y -> 
                let value = FSharpValue.GetRecordField(x,y)
                
                if value=null then
                    if y.PropertyType.IsGenericType && y.PropertyType.GetGenericTypeDefinition() = typedefof< Option<_>> then
                        strm.Write(0)
                    else
                        failwithf "Нельзя сериализовать значение null, %A, %A" tx.Name y 
                else                        
                    FSharpValue.GetRecordField(x,y) |> serialize  ) 
        else            
            sprintf "тип %A не сериализуем" tx.Name |> BinaryFormatException |> raise

[<AutoOpen>]
module private ``десериализация`` = 
    let rec deserialize (strm:BinaryReader) (tx:Type) : obj  =  
        let deserialize = deserialize strm    
        let fail() = failwithf "тип %A не сериализуем" tx.Name 
        if tx=typeof<string> then
            strm.ReadString() |> box
        elif tx=typeof<decimal> then
            strm.ReadDecimal() |> box
        elif tx=typeof<float> then
            strm.ReadSingle() |> box
        elif tx=typeof<single> then
            strm.ReadDouble() |> box
        elif tx=typeof<int8> then
            strm.ReadSByte() |> box
        elif tx=typeof<int16> then
            strm.ReadInt16() |> box
        elif tx=typeof<int32> then
            strm.ReadInt32() |> box
        elif tx=typeof<int64> then
            strm.ReadInt64() |> box

        elif tx=typeof<byte> then
            strm.ReadByte() |> box
        elif tx=typeof<uint16> then
            strm.ReadUInt16() |> box
        elif tx=typeof<uint32> then
            strm.ReadUInt32() |> box
        elif tx=typeof<uint64> then
            strm.ReadUInt64() |> box

        elif tx=typeof<bool> then
            strm.ReadBoolean() |> box
        elif tx=typeof<DateTime> then
            DateTime( strm.ReadInt64() ) |> box

        elif tx=typeof<byte []> then
            let len = strm.ReadInt32()
            let r = Array.create len 0xffuy
            let readedCount = strm.Read(r,0,len)
            if readedCount<>len then
                sprintf "неверный формат, не удалось прочитать массив %d байт, %d прочитано" len readedCount |> BinaryFormatException |> raise
            box r

        elif isEnumerable tx then 
            if strm.ReadString() <> "List" then
                sprintf "неверный формат, ожидался тип %A" (tx.Name) |> BinaryFormatException |> raise
            let len = strm.ReadInt32()
            if tx.IsArray then
                let valueType = tx.GetElementType()
                let result = Array.CreateInstance( valueType, len )
                for n in 0..(len-1) do  
                    result.SetValue( deserialize valueType , n )  
                box result
            elif tx.IsGenericType  then
                let deft = tx.GetGenericTypeDefinition()
                if deft = typedefof<list<_>> then      
                    let valueType = tx.GetGenericArguments().[0]          
                    makeListOf valueType [ for _ in 0..(len-1) ->  deserialize valueType ]

                elif deft = typedefof<Set<_>> then      
                    let valueType = tx.GetGenericArguments().[0]
                    let values = 
                        [   for _ in 0..(len-1) -> deserialize valueType ]
                    Activator.CreateInstance
                        ( tx,   makeListOf valueType values )
                elif deft = typedefof<Map<_,_>> then
                    let tkv = tx.GetGenericArguments()
                    let tkey = tkv.[0]
                    let tvalue = tkv.[1]
                    let keyValuePairType = typedefof<System.Tuple<_,_>>.MakeGenericType( tx.GetGenericArguments() )
                    let values = 
                        [   for _ in 0..(len-1) ->
                                let key = deserialize tkey
                                let value = deserialize tvalue
                                Activator.CreateInstance(keyValuePairType, [|key; value|]) ]
                    Activator.CreateInstance
                        ( tx,   makeListOf keyValuePairType values )
                else 
                    let valueType = tx.GetGenericArguments().[0]
                    let values = [ for _ in 0..(len-1) ->  deserialize valueType ]
                    Activator.CreateInstance( tx, makeListOf valueType values  )                
            else fail()
        elif  FSharpType.IsTuple tx then 
            FSharpValue.MakeTuple( FSharpType.GetTupleElements(tx) |> Array.map deserialize, tx)
        elif FSharpType.IsUnion tx then
            let cases = FSharpType.GetUnionCases tx
            let tag = strm.ReadInt32()
            let case = cases |> Array.find( fun x -> x.Tag=tag)
            let values = 
                case.GetFields() |> Array.map (fun px -> 
                deserialize px.PropertyType ) 
            FSharpValue.MakeUnion(case, values)
        elif FSharpType.IsRecord tx then
            let values = 
                FSharpType.GetRecordFields(tx)
                |> Array.map ( fun px -> deserialize px.PropertyType )
            FSharpValue.MakeRecord( tx, values)
        else fail()

let deserialize<'T> strm = deserialize strm typeof<'T> :?> 'T

