module TextConfig

open System
open System.IO

let readFromFile fileName foo = 
    if File.Exists fileName |> not then None else
    try
        match foo (File.ReadAllText(fileName)) with
        | Left x -> sprintf "Ошибка данных в файле %A\n%s" (Path.GetFileName fileName) x |> log.Error; None
        | Right x -> Some x
    with e ->
        sprintf "Ошибка считывания файла %A\n%A" (Path.GetFileName fileName) e |> log.Error
        None

let writeToFile fileName x = 
    try
        File.WriteAllText(fileName, x)
    with e ->
        sprintf "Ошибка записи файла %A\n%A" (Path.GetFileName fileName) e |> log.Error

let jsonFile<'a> configName dummy = 
    let dummy() = 
        printfn "%s- по умолчанию" configName
        dummy()

    let path =  IO.Path.Combine( exepath, sprintf "%s.config.json" configName)
    let config = 
    
        if IO.File.Exists path then 
            try
                match IO.File.ReadAllText(path) |> Json.toObj<'a> with
                | Right x -> x
                | Left x -> sprintf "ошибла файла конфигурации %s\n%s" ( Path.GetFileName path) x |> log.Error
                            dummy()
            with e ->
                sprintf "ошибла файла конфигурации %s\n%A" path e |> log.Error
                dummy()
        else
            dummy()
    let save() = config |> Json.fromObj |> writeToFile path
    config, save

