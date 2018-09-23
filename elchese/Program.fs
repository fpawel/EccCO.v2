open System
open System.Collections.Generic
open System.Data.SQLite
open System.Data

type Ctx = 
    {   conn : SQLiteConnection
        prod_types:Map<string,int64>
        exported_parties:Set<string>
        old_parties : DataModel.BatchInfo list
        repository_path: string
    }

    member x.migrate() = 
        printfn "exported parties: %A" x.exported_parties.Count
        printfn "product types: %A" x.prod_types.Count
        printfn "start export"
        let count = x.old_parties.Length
        for i = 0 to count - 1 do
            let old_party = x.old_parties.[i]
            let startTime = DateTime.Now
            x.export_party old_party.Id    
            printfn "%A: %A: %s, %A, №%d из %d, %d%s" 
                    DateTime.Now old_party.Date old_party.Id (DateTime.Now - startTime) (i+1) count
                    (int <| Math.Round ((float i / float count) * 100.)) "%"                  
        x.conn.Close()

    member x.export_party(old_party_id:DataModel.Id)  = 

        if Set.contains old_party_id x.exported_parties then () else

        let old_party = 
            match Repository.get x.repository_path old_party_id with
            | Err err -> failwith err 
            | Ok x -> x
    
        let cmd = new SQLiteCommand(x.conn)
    
        cmd.CommandText <- """
            BEGIN TRANSACTION;
            INSERT INTO party (created_at, old_party_id, product_type_id, conc1, conc2, conc3, note) 
                VALUES
                    (@created_at, @old_party_id, @product_type_id, @conc1, @conc2, @conc3, @note);
            SELECT last_insert_rowid();"""

        cmd.Parameters.Add("@created_at", DbType.DateTime).Value <- old_party.Date
        cmd.Parameters.Add("@old_party_id", DbType.String).Value <- old_party.Id
        cmd.Parameters.Add("@product_type_id", DbType.Int64).Value <- 
            match x.prod_types.TryFind old_party.ProductType with
            | Some x -> x
            | _ -> 1L
        cmd.Parameters.Add("@conc1", DbType.Decimal).Value <- old_party.PGS1
        cmd.Parameters.Add("@conc2", DbType.Decimal).Value <- old_party.PGS2
        cmd.Parameters.Add("@conc3", DbType.Decimal).Value <- old_party.PGS3
        if old_party.Name <> "" then 
            cmd.Parameters.Add("@note", DbType.String).Value <- old_party.Name
        else
            cmd.Parameters.Add("@note", DbType.Object).Value <- null      

        let party_id =cmd.ExecuteScalar() :?> int64
    
        for place = 0 to old_party.Products.Length - 1 do
            let old_product = old_party.Products.[place]
            x.export_product party_id place  old_product  old_party 
        
        let cmd = new SQLiteCommand(x.conn)
        cmd.CommandText <- "COMMIT;"
        let _ = cmd.ExecuteNonQuery()
        ()

    member x.export_product 
        (party_id:int64)
        (place:int)    
        (old_product:DataModel.Product)         
        (old_party:DataModel.Batch) = 
    
        match old_product.Serial with
        | None -> ()
        | Some serial ->

        let cmd = new SQLiteCommand(x.conn)

        let prmo (t:DbType) prm (v:'T option) =
            match v with
            | Some x -> 
                cmd.Parameters.Add(prm, t).Value <- x
            | _ -> 
                cmd.Parameters.Add(prm, DbType.Object).Value <- null

        let deco = prmo DbType.Decimal     
    
        cmd.CommandText <- """
                INSERT INTO product
                  (party_id, serial, place, product_type_id,  
                        i13, i24, i35, i26, i17, not_measured, 
                        flash, production, old_product_id, old_serial)
                VALUES        
                  (@party_id, @serial, @place, @product_type_id,  
                    @i13, @i24, @i35, @i26, @i17, @not_measured, 
                    @flash, @production, @old_product_id, @old_serial);
                    SELECT last_insert_rowid();"""

        cmd.Parameters.Add("@party_id", DbType.Int64).Value <- party_id
        if old_party.Products |> List.exists ( fun x -> x.Id <> old_product.Id && x.Serial = Some serial) then
            cmd.Parameters.Add("@serial", DbType.Object).Value <- null        
        else
            cmd.Parameters.Add("@serial", DbType.Int32).Value <- serial    
        cmd.Parameters.Add("@place", DbType.Int64).Value <- place
        prmo DbType.Int64 "@product_type_id" (x.prod_types.TryFind old_product.ProductType)
        deco "@i13" old_product.I13
        deco "@i24" old_product.I24
        deco "@i35" old_product.I35
        deco "@i26" old_product.I26
        deco "@i17" old_product.I17
        deco "@not_measured" old_product.In
        if old_product.Flash |> Array.exists( (<>) 0xffuy) then 
            cmd.Parameters.Add("@flash", DbType.Binary, old_product.Flash.Length).Value <- old_product.Flash
        else 
            cmd.Parameters.AddWithValue("@flash", null) |> ignore   
        cmd.Parameters.Add("@production", DbType.Boolean).Value <- old_product.IsReportIncluded
        cmd.Parameters.Add("@old_product_id", DbType.String).Value <- old_product.Id
        cmd.Parameters.Add("@old_serial", DbType.Int32).Value <- serial
        
        let new_product_id = cmd.ExecuteScalar() :?> int64

        [   for a in old_product.CustomTermo do
                yield ("f", a.T), a.I
                yield ("s", a.T), a.K
            yield ("f", 20M), old_product.Ifon
            yield ("s", 20M), old_product.Isns
            yield ("f", -20M), old_product.If_20
            yield ("s", -20M), old_product.Is_20
            yield ("f", 50M), old_product.If50
            yield ("s", 50M), old_product.Is50
        ]
        |> List.choose( fun (k,v) -> 
            match v with 
            | None -> None
            | Some v -> Some(k,v) )
        |> Map.ofList
        |> Map.toList

        |> List.iter (fun ( (sc,t),v) -> 
            let cmd = new SQLiteCommand(x.conn)
            cmd.CommandText <- """
    INSERT INTO product_current (product_id, scale, temperature, current)
    VALUES (@product_id, @scale, @temperature, @current);"""
            cmd.Parameters.Add("@product_id", DbType.Int64).Value <- new_product_id
            cmd.Parameters.Add("@scale", DbType.String).Value <- sc
            cmd.Parameters.Add("@temperature", DbType.Decimal).Value <- t
            cmd.Parameters.Add("@current", DbType.Decimal).Value <- v
            cmd.ExecuteNonQuery() |> ignore )
        
let newCtx() = 
    let fileName = @"E:\Program Data\Аналитприбор\elchese\elchese.sqlite"
    let repository_path = @"E:\User\Projects\VS2018\EccCO.v2\App\bin\Release" 
    let conn = new SQLiteConnection( sprintf "Data Source=%s; Version=3;" fileName)
    try 
        conn.Open() 
    with e ->
        failwith e.Message        
    printfn "open %s: ok" fileName  

    use cmd = new SQLiteCommand(conn)
    cmd.CommandText <- "PRAGMA foreign_keys = ON; PRAGMA encoding = 'UTF-8';"
    cmd.ExecuteNonQuery() |> ignore 

    use cmd = new SQLiteCommand(conn)
    cmd.CommandText <- "SELECT old_party_id FROM party;"
    use r = cmd.ExecuteReader()    
    let exported_parties =
        seq{
            while r.Read() do         
                yield r.["old_party_id"] :?> string            
        } |> Set.ofSeq
        

    use cmd = new SQLiteCommand(conn)
    cmd.CommandText <- "SELECT product_type_id, name FROM product_type;"
    use r = cmd.ExecuteReader()    
    let prod_types =
        seq{
            while r.Read() do         
                yield r.["name"] :?> string, r.["product_type_id"] :?> int64        
        } |> Map.ofSeq

    {   conn = conn
        prod_types  = prod_types
        exported_parties = exported_parties
        old_parties = 
            Repository.getInfoList repository_path
            |> List.sortBy(fun x -> x.Date.Ticks * (-1L))
        repository_path = repository_path 
    }
    
[<EntryPoint>]
let main argv = 
    let ctx = newCtx()
    ctx.migrate()
    printfn "Press any key..."
    let _ = Console.ReadKey()
    printfn "Buy!"
    printfn "%A" argv
    0 // return an integer exit code
