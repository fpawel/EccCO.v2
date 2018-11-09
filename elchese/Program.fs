open System
open System.Data.SQLite
open System.Data
open System.IO
open System.Diagnostics

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
    }

    member x.migrate() = 

        let cmd = new SQLiteCommand(x.conn, CommandText = "BEGIN TRANSACTION;")
        cmd.ExecuteNonQuery() |> ignore

        let startTime = DateTime.Now
    
        let percent  = 
            let count = x.old_parties.Length
            fun i ->                
                ((float i / float count) * 100.) |> round |> int

        let resetCursor =             
            let conx, cony = Console.CursorLeft, Console.CursorTop
            fun () ->
                let conx2 = Console.CursorLeft
                Console.SetCursorPosition(conx, cony)
                printf "%s" (String(' ', conx2 - conx + 1))
                Console.SetCursorPosition(conx, cony)            

        x.old_parties
        |> List.iteri(fun i old_party ->
            
            let s = sprintf "%d %d%s %A  " (i+1) ( percent i) "%" old_party.Date

            if Set.contains old_party.Id x.exported_parties then
                resetCursor()
                printf "%s: skip" s
            else
                x.export_party old_party.Id  old_party.Date
                resetCursor()
                printf "%s" s 
            )         
        cmd.CommandText <- "COMMIT;"
        cmd.ExecuteNonQuery() |> ignore
        printfn ""
        printfn "total time: %A" (DateTime.Now - startTime)

        

    member x.export_party old_party_id old_party_date = 

        let old_party = 
            match Repository.loadParty Environment.CurrentDirectory old_party_id old_party_date with
            | Err err -> failwith err 
            | Ok x -> x
        
        let cmd = new SQLiteCommand(x.conn)
    
        cmd.CommandText <- """
            
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
        if old_party.Products |> List.exists ( fun x -> x.N < old_product.N && x.Serial = Some serial) then
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

open FSharp.Data.Sql

let [<Literal>] resolutionPath = @"C:\Users\fpawel\AppData\Roaming\Аналитприбор\elchese\elchese.sqlite" 
let [<Literal>] connectionString = "Data Source=" + resolutionPath

// create a type alias with the connection string and database vendor settings
type sql = SqlDataProvider< 
              ConnectionString = connectionString,
              DatabaseVendor = Common.DatabaseProviderTypes.SQLITE,
              ResolutionPath = resolutionPath,
              IndividualsAmount = 1000,
              UseOptionTypes = true >


        
[<EntryPoint>]
let main argv = 
    let ctx = sql.GetDataContext()
    let xs  = 
        query{
            for c in ctx.Main.Gas do 
                let s = c.GasName.Value
                yield s
        } |> Seq.toArray

    printfn "%s" <| String.Join(" ", xs )
        
    


    //let fileName = @"E:\Program Data\Аналитприбор\elchese\elchese.sqlite"
    //let repository_path = @"E:\User\Projects\VS2018\EccCO.v2\App\bin\Release" 
    // @"C:\Users\fpawel\Documents\Visual Studio 2015\Projects\Analit\EccCO.v2\App\bin\Release" 

    let folderName = 
        let mutable s = Environment.GetEnvironmentVariable("MYAPPDATA") 
        if s = "" then
            s <- Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData)
        Path.Combine(s, "Аналитприбор", "elchese")

    let fileName = Path.Combine(folderName, "elchese.sqlite" )    

    let fileExitst = File.Exists fileName

    printfn "database: %s" fileName  
    if not fileExitst then
        printfn "create sqlite database file"
        SQLiteConnection.CreateFile fileName
        
    let conn = new SQLiteConnection( sprintf "Data Source=%s; Version=3;" fileName)
    try 
        conn.Open() 
    with e ->
        failwith e.Message        
    
    printfn "%A: initialize sqlite database" DateTime.Now
    let cmd = new SQLiteCommand(conn, CommandText = initdb.sql)
    cmd.ExecuteNonQuery() |> ignore

    printfn "%A: read exported parties" DateTime.Now
    cmd.CommandText <- "SELECT old_party_id FROM party WHERE old_party_id NOT NULL;"
    use r = cmd.ExecuteReader()    
    let exported_parties =
        seq{
            while r.Read() do         
                yield r.["old_party_id"] :?> string            
        } |> Set.ofSeq

    printfn "exported parties: %d" exported_parties.Count
    
    printfn "%A: read old parties" DateTime.Now
    let old_parties = 
        Repository.getInfoList Environment.CurrentDirectory
        |> List.sortBy(fun x -> x.Date.Ticks * (-1L))
    printfn "%A: old parties: %d" DateTime.Now old_parties.Length 

    printfn "%A: start export" DateTime.Now
        
    
    {   conn = conn
        exported_parties = exported_parties
        old_parties = old_parties
    }.migrate()

    conn.Close()

    Process.Start(folderName) |> ignore
    printfn "Press any key..."
    let _ = Console.ReadKey()
    printfn "Buy!"
    0
    
