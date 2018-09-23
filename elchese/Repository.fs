module Repository

open System
open System.IO
open DataModel

type Id = string

[<AutoOpen>]
 module private Helpers = 
    let usefile<'a> (path:string) (x:FileMode) (f : FileStream -> 'a)= 
        try
            use file = new FileStream( path, x ) 
            f file |> Ok
        with e ->             
            printfn "Ошибка обращения к файлу %s, %A, %A" path x e 
            Err e.Message

    type FileName = { FileName : string }

    let readFile<'T> filename (func : BinaryReader -> 'T)  =        
        usefile filename.FileName FileMode.Open <| fun file ->
            use reader = new BinaryReader(file)
            func reader 

    module Path = 
        let data rootPath = 
            let x = Path.Combine( rootPath, "Batches" ) 
            if Directory.Exists x |> not then 
                Directory.CreateDirectory x |> ignore
            x
        
        let batchFolder rootPath canCreate id (dt:DateTime)  = 
            let (~%%) = string
            let month = dt.ToString("MMM", System.Globalization.CultureInfo.InvariantCulture)
            let path = Path.Combine(data rootPath, %% dt.Year, sprintf "%d-%s" dt.Month month, %% dt.Day, id )
            if canCreate then
                createDirectory path
            { FileName = path }

        let batchFileName rootPath canCreate id dt  =             
            { FileName = Path.Combine( (batchFolder rootPath canCreate id dt).FileName, sprintf "%s.batch"  id ) }
            
    let getBatchesInfo( rootPath ) =
        let files = Directory.GetFiles( Path.data rootPath, "*.batch", SearchOption.AllDirectories)
        let r = 
            [   for filename in files do
                    match   readFile {FileName=filename} FSharpBin.deserialize<BatchInfo>  with
                    | Err x ->  ()                  
                    | Ok x -> yield x ]   
        r


module Path =
    let year rootPath year = 
        let x = Path.batchFolder rootPath false "-" (DateTime(year ,1,1) )
        x.FileName 
        |> Path.GetDirectoryName
        |> Path.GetDirectoryName
        |> Path.GetDirectoryName
    let month rootPath year month = 
        let x = Path.batchFolder rootPath false "-" (DateTime(year,month,1) )
        x.FileName 
        |> Path.GetDirectoryName
        |> Path.GetDirectoryName

    let day rootPath year month day = 
        let x = Path.batchFolder rootPath false "-" (DateTime(year,month,day) )
        x.FileName 
        |> Path.GetDirectoryName

    let batch rootPath id dateTime = 
        (Path.batchFolder rootPath false id dateTime).FileName
  
[<AutoOpen>]
module private Helpers1 = 
    let load rootPath id dt =
        let fileName = Path.batchFileName rootPath false id dt
        if File.Exists fileName.FileName |> not then
            Err ("не найден файл " + fileName.FileName)
        else
            readFile fileName <| fun reader ->
                let x = FSharpBin.deserialize<Batch> reader
                {   x with 
                        Products = 
                            [   for p in x.Products -> 
                                    if p.Flash.Length = EEPROM_SIZE then p else
                                    { p with Flash = Array.create EEPROM_SIZE 0xffuy}] }
                
                

    let tryGetBatchInfo rootPath id = 
        match getBatchesInfo rootPath |> List.tryFind( fun x -> x.Id=id) with
        | None -> None
        | Some batchInfo -> Some batchInfo

    
            


let getInfoList rootPath = 
    List.rev ( getBatchesInfo rootPath)


let get rootPath id =
    match tryGetBatchInfo rootPath id with
    | None -> Err "партия не найдена"
    | Some batchInfo ->
        load rootPath id batchInfo.Date 




   