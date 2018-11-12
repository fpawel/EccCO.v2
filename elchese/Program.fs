open System
open System.Data.SQLite
open System.IO
open System.Diagnostics

        
[<EntryPoint>]
[<STAThread>] 
let main argv =    
    EccCO.v2.Config.setup()
    db.syncronize()      
    printfn "Press any key..."
    let _ = Console.ReadKey()
    printfn "Buy!"
    0
    
