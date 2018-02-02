module Prog

let mutable removeErrorMessage = fun () -> ()

let private doTransfert<'a> cmd data f = 
    match Mdbs.doTransfer<'a> AppSets.sets.ComportProg cmd data f with
    | Left error -> 
        removeErrorMessage <- UI.Log.Jouranl.addError error ( sprintf "%X:%s" cmd (SerialPorts.showBytes data) ) 
        Left error
    | Right x -> 
        Right x

let private (~%%) (n:int) = 
    let n = uint16 n
    [  byte (n >>> 8); byte n ]        

let write n data addy is64 =
    let cmd = if is64 then 1uy else 5uy
    let txd = 
        [   yield byte n
            yield! %% addy
            yield! %% Array.length data
            yield! data ]
    doTransfert cmd txd Right
    |> leftSome

let read n addy count is64 =
    let cmd = if is64 then 2uy else 6uy
    let txd = 
        [   yield byte n
            yield! %% addy
            yield! %% count ]
    doTransfert cmd txd Right

