module SerialPorts

open System.IO.Ports
open AppSets

module Port =
    let mutable serialPorts : Map<string,SerialPort * string > = Map.empty
    
    let getPort sets = 
        match serialPorts.TryFind sets.PortName with
        | Some (port,descr) when descr<> sets.Description ->             
            serialPorts <- Map.add sets.PortName (port,sets.Description) serialPorts
            Right port
        | Some (port,_) -> Right port
        | _ ->
            try
                let newport = new SerialPort(sets.PortName)
                serialPorts <- Map.add sets.PortName (newport,sets.Description) serialPorts
                Right newport
            with e -> Left e.Message               

    let getPortByName portName =        
        match serialPorts |> Map.tryFind portName with
        | Some (port,_) -> Right port
        | _ ->  
            try
                new SerialPort(portName) |> Right
            with e -> Left e.Message

    let getPorts =  
        serialPorts |> Map.toList |> List.map( fun( portName, (_,descr)) -> portName, descr)

    let closeOpenedPorts( pred ) =  
        serialPorts |> Map.iter ( fun portName (port, descr) -> 
            if port.IsOpen && (pred portName descr) then 
                port.DiscardInBuffer()
                port.DiscardOutBuffer()
                port.Close() )
        serialPorts <- Map.empty

    let getPortsNames = ""::(System.IO.Ports.SerialPort.GetPortNames() |> Array.toList)

[<AutoOpen>]
module private Helpers1 = 

    let bytesToStrings bytes =      
        Seq.fold ( fun (acc,i) b ->
            let s = sprintf "%s%X" (if b<0x10uy then "0" else "") b
            if i%16=0 then (s::acc, i+1) else                     
            match acc with 
            | [] -> ( s::[], i+1) 
            | s'::[] -> ((s'+" "+s)::[], i+1)
            | s'::acc  -> ((s'+" "+s)::acc, i+1)) ( [], 0 ) bytes |> fst |> List.rev

let showBytes bytes = Seq.fold ( fun acc s -> if acc="" then s else acc + " "+s) "" (bytesToStrings bytes)

[<AutoOpen>]
module private Helpers2 = 


    let formatError canLog (portName : string) (msg : string) txd = 
        if msg.Contains portName then msg else 
        sprintf "COM порт %A, %s%s" 
            portName msg 
            (if canLog then sprintf ", %s" <| showBytes txd else "")

    let tickcount() = System.Environment.TickCount

    let sleep (n:int) = 
        let x0 = tickcount()
        while tickcount() - x0 < n  do
            System.Threading.Thread.Sleep 10

    let discardBuffers (port:SerialPort) = 
        port.DiscardInBuffer()
        port.DiscardOutBuffer()

    let applyPort sets = 
        if SerialPort.GetPortNames() |> Set.ofArray |> Set.contains sets.PortName |> not then 
            (sprintf "Недопустимое имя СОМ порта %A" sets.PortName ) |> Left else
        match Port.getPort sets with
        | Left x -> Left x
        | Right port ->
            let _open() = 
                port.PortName <- sets.PortName
                port.BaudRate <- sets.BaudRate
                port.Open()
            if not port.IsOpen then _open() 
            elif port.PortName<>sets.PortName || port.BaudRate<>sets.BaudRate then
                port.Close()
                _open()
            discardBuffers port
            Right port

    

    let doWithLogTime<'a> what (x:unit -> 'a) = 
         let t0 = tickcount()
         let x = x()
         sprintf "%s %d мс" what (tickcount() - t0) |> log.Info
         x

    let writeToPort (port:SerialPort) (txd:byte seq)  =
        //discardBuffers port
        let txd = Seq.toArray txd
        //doWithLogTime "port.Write" <| fun () -> 
        port.Write( txd, 0, txd.Length)

    let checkCondWithTimeout timeout cond  work = 
        let t0 = tickcount()
        let rec loop() = 
            work()
            if  (cond() |> not) &&  (tickcount() - t0 < timeout) then loop() 
        loop()

let test portName = 
    match Port.getPortByName portName with
    | Left x -> Some x
    | Right port ->
        if port.IsOpen then None else
        try 
            port.Open()
            port.Close()
            None
        with  e -> Some e.Message
    
   
let write sets (txd:byte seq)  =
    let fail error = formatError sets.CanLog sets.PortName error txd |> Some
    try
        match applyPort sets with
        | Left error -> fail error
        | Right port ->
            if sets.CanLog then showBytes txd |> sprintf "%s <== %s" sets.PortName |> log.Info 
            let txd = Seq.toArray txd
            port.Write( txd, 0, txd.Length)
            None
    with e ->
        fail e.Message

let sndrecv sets txd : Either<string, byte[]>=     
    let rec loop (port:SerialPort) n = 
        
        writeToPort port txd
        let tickStart = tickcount()
        let rec reciving recived = 
            let hasBytes() = port.BytesToRead>0
            checkCondWithTimeout sets.Timeout hasBytes ( fun() -> sleep 1 )
            if port.BytesToRead=0 then recived else
            let rxdlen = port.BytesToRead
            let oldlen = recived |> Array.length
            let recived = [| yield! recived; yield! Array.create rxdlen 0uy |]            
            if port.Read(recived, oldlen, rxdlen) <> rxdlen then 
                failwith "Ошибка считывания из %s, %s" sets.PortName (showBytes txd)
            checkCondWithTimeout (max sets.Chartime 1) hasBytes ( fun() -> sleep 5 )
            if port.BytesToRead=0 then recived else reciving recived
        let rxd = reciving [||]
        let ms = tickcount()-tickStart
        if sets.CanLog then
            if rxd.Length>0 then            
                log.Info ( sprintf "%s, %s ==> %s, %d мс" sets.PortName (showBytes txd) (showBytes rxd) ms )
            else
                log.Debug ( sprintf "%s, нет ответа, %d мс, %s" sets.PortName ms (showBytes txd) )
        if rxd |> Array.isEmpty |> not then
            sleep sets.Delay
            rxd
        else if n < sets.RepeatCount then loop port (n+1) else [||]
    try
        match applyPort sets with
        | Left error -> formatError sets.CanLog sets.PortName error txd |> Left
        | Right port ->
            loop port 0 |> Right
    with e ->
        formatError sets.CanLog sets.PortName e.Message txd |> Left









