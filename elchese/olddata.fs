namespace olddata

open System.Collections.Generic

type Entities (path) =
    
    member __.Parties() : IEnumerable<DataModel.BatchInfo> = 
        Repository.getInfoList path
        |> Seq.cast

    member __.Party(id) : DataModel.Batch = 
        Repository.get path id 
        |> Either.unwrap 
        