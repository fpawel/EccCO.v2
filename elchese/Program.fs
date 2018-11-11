open System
open System.Data.SQLite
open System.IO
open System.Diagnostics

        
[<EntryPoint>]
let main argv =    
    // E:\User\Projects\VS2018\EccCO.v2\App\bin\Release
    // C:\Users\fpawel\Documents\Visual Studio 2015\Projects\Analit\EccCO.v2\App\bin\Release\
    db.syncronize()      
    printfn "Press any key..."
    let _ = Console.ReadKey()
    printfn "Buy!"
    0
    
