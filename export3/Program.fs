open System
open System
open System.Collections.Generic
open System.Data.SQLite

let mutable rootPath = exepath 


let getExportedParties(conn : SQLiteConnection) = 
    let cmd = new SQLiteCommand(conn)
    cmd.CommandText <- "SELECT import_id FROM import_parties;"
    let r = cmd.ExecuteReader()    
    seq{
        while r.Read() do         
            yield r.["import_id"] :?> string            
    } |> Set.ofSeq
    
    
let getProductTypes(conn : SQLiteConnection) = 
    let cmd = new SQLiteCommand(conn)
    cmd.CommandText <- "SELECT product_type_id, product_type_name FROM product_types;"
    let r = cmd.ExecuteReader()    
    let xs = Dictionary<string, int64>()    
    while r.Read() do         
        xs.[r.["product_type_name"] :?> string] <- r.["product_type_id"] :?> int64        
    Map.ofSeq <| seq{ for x in xs -> x.Key, x.Value }
    

let exportProduct (conn : SQLiteConnection) 
    (p:DataModel.Product) 
    (partyDate:DateTime)
    (orderInParty:int)
    (partyID:int64) 
    (prodTypes:Map<string,int64>) = 
    
    match p.Serial with
    | None -> ()
    | Some serial ->
    
    let prodType = 
        match prodTypes.TryFind p.ProductType with
        | Some x -> sprintf "%d" x
        | None -> "NULL"
                     
    let f d = 
        match d with
        | Some x -> sprintf "%M" x
        | _ -> "NULL"
        

    let cmd = new SQLiteCommand(conn)
    let dateStr = partyDate.ToString("yyyy-MM-dd HH:mm") 
    let prod = if p.IsReportIncluded then 1 else 0
    
    
    cmd.CommandText <- 
        sprintf """
INSERT INTO products
  (party_id, serial_number, order_in_party, product_type_id,  
        fon20, sens20, i13, i24, i35, i26, i17, not_measured, 
        fon_minus20, sens_minus20, fon50, sens50, flash, updated_at,  production)
VALUES        
  (%d, %d, %d, %s,  
    %s, %s, %s, %s, %s, %s, %s, %s,  
    %s, %s, %s, %s, @flash, datetime ('%s'), %d);"""
         partyID serial orderInParty prodType 
         (f p.Ifon) (f p.Isns) (f p.I13) (f p.I24) (f p.I35) (f p.I26) (f p.I17) (f p.In)
         (f p.If_20) (f p.Is_20) (f p.If50) (f p.Is50) dateStr prod
    if p.Flash |> Array.exists( (<>) 0xffuy) then 
        cmd.Parameters.Add("@flash", System.Data.DbType.Binary, p.Flash.Length).Value <- p.Flash
    else 
        cmd.Parameters.AddWithValue("@flash", "NULL") |> ignore   
            
    cmd.ExecuteNonQuery() |> ignore     
    
let exportParty (conn : SQLiteConnection) (i:DataModel.BatchInfo) (prodTypes:Map<string,int64>) = 
    
    let prodTypeID = 
        match prodTypes.TryFind i.ProductType with
        | Some x -> x
        | _ -> 1L
        
    let party = match Repository.get rootPath i.Id with
                | Left err -> failwith err 
                | Right x -> x
    
        
    let dateStr = i.Date.ToString("yyyy-MM-dd HH:mm") 
    let cmd = new SQLiteCommand(conn)
    let note = if i.Name = "" then "NULL" else  i.Name
    
    
    cmd.CommandText <- sprintf """
        BEGIN TRANSACTION;
        INSERT INTO parties (created_at, product_type_id, gas1, gas2, gas3, note) 
            VALUES
                (datetime ('%s'), %d, %M, %M, %M, '%s');
        SELECT last_insert_rowid();"""
                            dateStr prodTypeID party.PGS1 party.PGS2 party.PGS3 note
    let partyID =cmd.ExecuteScalar() :?> int64
    
    for n = 0 to party.Products.Length - 1 do
        let p = party.Products.[n]
        exportProduct conn p i.Date n partyID prodTypes  
             
            
    
    let cmd = new SQLiteCommand(conn)
    cmd.CommandText <- 
        sprintf 
            """INSERT INTO import_parties (import_id, party_id) VALUES ('%s',%d);
               COMMIT; 
            """
                i.Id partyID
    let _ = cmd.ExecuteNonQuery()
    ()
    

[<EntryPoint>]
let main argv =    
    if argv.Length = 1 then 
        rootPath <- argv.[0].Substring(0, argv.[0].Length - 1)
    let appdataFolderPath = Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData)
    let fileName = IO.Path.Combine(appdataFolderPath, "Аналитприбор", "eccco73", "products.db") 
    let conn = new SQLiteConnection( sprintf "Data Source=%s; Version=3;" fileName)
    try 
        conn.Open() 
    with e ->
        failwith e.Message        
    printfn "open %s: ok" fileName    
    
    let exportedParties = getExportedParties conn    
    
    
    let productTypes = getProductTypes conn
    printfn "exportedParties: %A" exportedParties.Count
    printfn "productTypes: %A" productTypes.Count
    printfn "Starting export..."
    let parties = Repository.getInfoList rootPath
    for i = 0 to parties.Length - 1 do
        let p = parties.[i]
        if not <| Set.contains p.Id exportedParties then 
            let startTime = DateTime.Now
            exportParty conn p productTypes    
            printfn "%A: %A: %s, %A, №%d из %d, %d%s" 
                DateTime.Now p.Date p.Id (DateTime.Now - startTime) (i+1) parties.Length
                (int <| Math.Round ((float i / float parties.Length) * 100.)) "%"  
                
    conn.Close()
    printfn "Press any key..."
    let _ = Console.ReadKey()
    printfn "Buy!"
    0 // возвращение целочисленного кода выхода
