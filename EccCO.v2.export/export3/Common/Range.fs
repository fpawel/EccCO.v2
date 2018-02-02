module Range

let rng x delim = 
    let rec rng acc (x1,x2) delim = 
        let n1 = x1 / delim
        let n2 = x2 / delim
        if n1 = n2 then (x1,x2)::acc else
        let v = x1 + (delim - (x1 % delim) )
        rng ( (x1,v-1)::acc ) (v,x2) delim
    rng [] x delim |> List.rev

let mkrng (rngs : (int*int) list) delim =
    let rngs = List.sort rngs        
    [   for x in rngs do
            yield! rng x delim ]

let count (rngs : (int*int) list) =
    let rec loop acc = function            
        | [] -> acc
        | (x1,x2)::left -> loop (x2-x1+1 + acc) left
    loop 0 rngs