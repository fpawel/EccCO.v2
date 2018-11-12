namespace EccCO.v2 

module Repository =

    open System
    open System.IO
    open DataModel

    

    type Id = string

    

    

    [<AutoOpen>]
    module private Helpers = 
        
        type FileName = { FileName : string }

        let readFile<'T> filename (f : BinaryReader -> 'T)  =   
            try
                use file = new FileStream( filename.FileName, FileMode.Open ) 
                use reader = new BinaryReader(file)
                f reader 
            with e ->             
                failwithf "Ошибка обращения к файлу %s, %A" filename.FileName e 
        
        

        module Path = 
            let data () = 
                let x = Path.Combine( Config.rootPath, "Batches" ) 
                if Directory.Exists x |> not then 
                    Directory.CreateDirectory x |> ignore
                x
        
            let batchFolder canCreate id (dt:DateTime)  = 
                let (~%%) = string
                let month = dt.ToString("MMM", System.Globalization.CultureInfo.InvariantCulture)
                let path = Path.Combine(data(), %% dt.Year, sprintf "%d-%s" dt.Month month, %% dt.Day, id )
                if canCreate then
                    createDirectory path
                { FileName = path }

            let batchFileName canCreate id dt  =             
                { FileName = Path.Combine( (batchFolder canCreate id dt).FileName, sprintf "%s.batch"  id ) }
            
        let getBatchesInfo() =
            let files = Directory.GetFiles( Path.data(), "*.batch", SearchOption.AllDirectories)
            let r = 
                [   for filename in files ->
                        readFile {FileName=filename} FSharpBin.deserialize<BatchInfo> ]   
            r

           

    module Path =
        let year year = 
            let x = Path.batchFolder false "-" (DateTime(year ,1,1) )
            x.FileName 
            |> Path.GetDirectoryName
            |> Path.GetDirectoryName
            |> Path.GetDirectoryName
        let month year month = 
            let x = Path.batchFolder false "-" (DateTime(year,month,1) )
            x.FileName 
            |> Path.GetDirectoryName
            |> Path.GetDirectoryName

        let day year month day = 
            let x = Path.batchFolder false "-" (DateTime(year,month,day) )
            x.FileName 
            |> Path.GetDirectoryName

        let batch id dateTime = 
            (Path.batchFolder false id dateTime).FileName
        
    let loadParty id dt =
        let fileName = Path.batchFileName false id dt
        if File.Exists fileName.FileName |> not then
            failwithf "не найден файл %A" fileName.FileName
        readFile fileName <| fun reader ->
            let x = FSharpBin.deserialize<Batch> reader
            {   x with 
                    Products = 
                        [   for p in x.Products -> 
                                if p.Flash.Length = EEPROM_SIZE then p else
                                { p with Flash = Array.create EEPROM_SIZE 0xffuy}] }
        
    let tryGetBatchInfo id = 
        match getBatchesInfo() |> List.tryFind( fun x -> x.Id=id) with
        | None -> None
        | Some batchInfo -> Some batchInfo

    let getInfoList () = 
        List.rev ( getBatchesInfo ())

    
    let get id =
        match tryGetBatchInfo id with
        | None -> failwith "партия не найдена"
        | Some batchInfo ->
            loadParty id batchInfo.Date 

    let save batch = 
        let batchInfo = BatchInfo.creteNew batch
        let fileName = Path.batchFileName true batchInfo.Id batchInfo.Date
        try
            let fileStream = new FileStream( fileName.FileName, FileMode.Create ) 
            let writer = new BinaryWriter(fileStream)
            FSharpBin.serialize writer batch
            writer.Close()
            fileStream.Close()

        with e ->             
            failwithf "write file %A: %s" fileName.FileName e.Message 
            




   