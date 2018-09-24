open System
open System.Collections.Generic
open System.Data.SQLite
open System.Data

let cmd_prmo (cmd : SQLiteCommand) (t:DbType) prm (v:'T option) =        
        match v with
        | Some a -> 
            cmd.Parameters.Add(prm, t).Value <- a
        | _ -> 
            cmd.Parameters.Add(prm, DbType.Object).Value <- null

let cmd_deco cmd = 
        cmd_prmo cmd DbType.Decimal


type Ctx = 
    {   conn : SQLiteConnection
        exported_parties:Set<string>
        old_parties : DataModel.BatchInfo list
        repository_path: string
    }

    member x.migrate() = 
        printfn "exported parties: %A" x.exported_parties.Count
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
            INSERT INTO party (created_at, old_party_id, product_type_name, conc1, conc2, conc3, note) 
                VALUES
                    (@created_at, @old_party_id, @product_type_name, @conc1, @conc2, @conc3, @note);
            SELECT last_insert_rowid();"""

        cmd.Parameters.Add("@created_at", DbType.DateTime).Value <- old_party.Date
        cmd.Parameters.Add("@old_party_id", DbType.String).Value <- old_party.Id
        cmd.Parameters.Add("@product_type_name", DbType.String).Value <- old_party.ProductType        
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
        cmd.CommandText <- """
                INSERT INTO product
                  (party_id, serial, place, product_type_name,  
                        i13, i24, i35, i26, i17, not_measured, 

                        i_f_minus20, i_f_plus20, i_f_plus50, i_s_minus20, i_s_plus20, i_s_plus50,

                        flash, production, old_product_id, old_serial)
                VALUES        
                  (@party_id, @serial, @place, @product_type_name,  
                    @i13, @i24, @i35, @i26, @i17, @not_measured, 
                    @i_f_minus20, @i_f_plus20, @i_f_plus50, @i_s_minus20, @i_s_plus20, @i_s_plus50,
                    @flash, @production, @old_product_id, @old_serial);
                    SELECT last_insert_rowid();"""

        cmd.Parameters.Add("@party_id", DbType.Int64).Value <- party_id
        if old_party.Products |> List.exists ( fun x -> x.Id <> old_product.Id && x.Serial = Some serial) then
            cmd.Parameters.Add("@serial", DbType.Object).Value <- null        
        else
            cmd.Parameters.Add("@serial", DbType.Int32).Value <- serial    
        cmd.Parameters.Add("@place", DbType.Int64).Value <- place        
        
        cmd_prmo cmd DbType.String "@product_type_name" (if old_product.ProductType = "" then None else Some old_product.ProductType)

        cmd_deco cmd "@i13" old_product.I13
        cmd_deco cmd "@i24" old_product.I24
        cmd_deco cmd "@i35" old_product.I35
        cmd_deco cmd "@i26" old_product.I26
        cmd_deco cmd "@i17" old_product.I17
        cmd_deco cmd "@not_measured" old_product.In

        cmd_deco cmd "@i_f_minus20" old_product.If_20
        cmd_deco cmd "@i_f_plus20" old_product.Ifon
        cmd_deco cmd "@i_f_plus50" old_product.If50
        cmd_deco cmd "@i_s_minus20" old_product.Is_20
        cmd_deco cmd "@i_s_plus20" old_product.Isns
        cmd_deco cmd "@i_s_plus50" old_product.Is50

        if old_product.Flash |> Array.exists( (<>) 0xffuy) then 
            cmd.Parameters.Add("@flash", DbType.Binary, old_product.Flash.Length).Value <- old_product.Flash
        else 
            cmd.Parameters.AddWithValue("@flash", null) |> ignore   
        cmd.Parameters.Add("@production", DbType.Boolean).Value <- old_product.IsReportIncluded
        cmd.Parameters.Add("@old_product_id", DbType.String).Value <- old_product.Id
        cmd.Parameters.Add("@old_serial", DbType.Int32).Value <- serial
        
        let new_product_id = cmd.ExecuteScalar() :?> int64

        [   for a in old_product.CustomTermo do
                yield a.T, (a.I, a.K)
        ]
        |> Map.ofList
        |> Map.toList
        |> List.iter (fun ( t,(i,k)) -> 
            let cmd = new SQLiteCommand(x.conn)
            cmd.CommandText <- """
    INSERT INTO product_temperature_current_k_sens (product_id, temperature, current, k_sens)
    VALUES (@product_id, @temperature, @current, @k_sens);"""
            cmd.Parameters.Add("@product_id", DbType.Int64).Value <- new_product_id
            cmd.Parameters.Add("@temperature", DbType.Decimal).Value <- t
            cmd_deco cmd "@current" i
            cmd_deco cmd "@k_sens" k
            cmd.ExecuteNonQuery() |> ignore )
        
let newCtx() = 
    //let fileName = @"E:\Program Data\Аналитприбор\elchese\elchese.sqlite"
    //let repository_path = @"E:\User\Projects\VS2018\EccCO.v2\App\bin\Release" 

    let fileName = @"C:\Users\fpawel\AppData\Roaming\Аналитприбор\elchese\elchese.sqlite"
    let repository_path = @"C:\Users\fpawel\Documents\Visual Studio 2015\Projects\Analit\EccCO.v2\App\bin\Release" 

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
        

    {   conn = conn
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
