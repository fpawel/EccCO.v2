module Stend

let mutable removeErrorMessage = fun () -> ()

let private logError1 (error:string) bytes = 
    removeErrorMessage <- UI.Log.Jouranl.addError error (SerialPorts.showBytes bytes) 


let private logError (error:string) = 
    logError1 ( sprintf "Ошибка связи со стендом - %s" (error.ToLower()) ) 

let private read<'a> txd needLen (f : byte [] -> 'a) = 
    match SerialPorts.sndrecv AppSets.sets.ComportECC txd  with
    | Left error -> 
        logError error txd
        Left error
    | Right rxd ->
        if Array.isEmpty rxd then 
            logError1 "Стенд не отвечает" txd
            Left "Стенд не отвечает"
        elif rxd.Length<needLen then 
            logError ( sprintf "Длина посылки ответа меньше %d" needLen ) txd
            Left  "Ошибка связи со стендом"
        else
            Right (f rxd)

let readCurrent n = read [4uy; 3uy; byte n; 4uy+3uy+(byte n)] 4 <| fun rxd ->
    let (~%%) n = decimal rxd.[n]    
    let ui = abs( (%% 2)*256m+ (%% 3) - 10000m) * 0.1m
    let ui = if rxd.[1] = 1uy then (-ui) else ui
    let i_ya = ui/10m
    let i_ya = if( abs i_ya < 4m ) then i_ya * 0.5m else i_ya
    i_ya

let readTemperature() = read [3uy; 5uy; 3uy+5uy] 3 <| fun rxd ->
    let b1 = rxd.[1]
    let b2 = rxd.[2]        
    if b1=0uy then (decimal b2)/2m else -(decimal((~~~ b2)+1uy) )/2m