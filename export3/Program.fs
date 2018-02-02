open System

// Дополнительные сведения о F# см. на http://fsharp.org
// Дополнительную справку см. в проекте "Учебник по F#".

[<EntryPoint>]
let main argv = 
    let appdataFolderPath = Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData)
    let fileName = IO.Path.Combine(appdataFolderPath, "Аналитприбор", "eccco73", "products.db") 
    printfn "%A" fileName
    Console.ReadKey()
    0 // возвращение целочисленного кода выхода
