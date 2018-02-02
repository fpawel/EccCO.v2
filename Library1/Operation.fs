namespace HierarchicalOperationViewModels


open System
open System.Collections.ObjectModel


type Op1 =
    | Single of (unit -> string option) * string 
    | Scenary of (Op1 list) * string 


[<PropertyChanged.ImplementPropertyChanged>]
type OperationState = 
    {   mutable Checked : Nullable<bool> 
        mutable HandleChecked : bool }

[<PropertyChanged.ImplementPropertyChanged>]
type Operation = 
    {   Parent : Operation option
        mutable Items : Operation list
        Name : string
        Action : (unit -> string option) option
        State : OperationState        
        mutable Perform : ICommand
        mutable IsFreezed : bool}

    member x.parents =
        let rec loop acc x = 
            match x.Parent with 
            | Some x -> loop (x::acc) x                
            | _ -> acc
        loop [] x |> List.rev

    member x.tree =  
        [   for x in x.Items do 
                yield x 
                yield! x.tree   ]

    member x.freez() = 
        if x.IsFreezed then
            failwithf "Нелльзя повторно \"заморозить\" объект %A" x
        x.IsFreezed <- true
        subscribePropertyChanged x <| fun e ->
            failwithf "Нелльзя измениять свойство %A объекта %A" e.PropertyName x
        x

    member private x.validateCheckedParents() = 
        for x in x.parents do 
            let items = x.tree
            if items |> List.forall( fun x -> x.State.Checked=BoolTrue) then x.State.Checked <- BoolTrue
            if items |> List.forall( fun x -> x.State.Checked=BoolFalse) then x.State.Checked <- BoolFalse
            elif items |> List.exists( fun x -> x.State.Checked=BoolTrue) && 
                 items |> List.exists( fun x -> x.State.Checked=BoolFalse) then x.State.Checked <- BoolNone

    member x.FullName = 
        x::x.parents |> List.rev |> List.fold( fun acc x  -> 
            let what = x.Name
            if what="" then acc else            
            let what = if what.EndsWith "." then what.Substring(0,what.Length-1) else what 
            acc + ( if acc="" then "" else ". ") + ( sprintf "%s" what) ) ""

    static member createNew perform parent items name f = 
        let x = 
            {   IsFreezed = false
                Parent  = parent
                Items  = items
                Name  = name
                Action = f                
                Perform = null
                State = { Checked = BoolTrue 
                          HandleChecked = true} }
        x.Perform <- wpfCommnad1 <| fun () -> 
            perform x
        subscribePropertyChanged x.State <| fun e ->
            if x.State.HandleChecked && e.PropertyName="Checked"  then                
                if x.State.Checked <> BoolNone then
                    x.tree |> List.iter( fun y ->                     
                        y.State.HandleChecked <- false
                        y.State.Checked <- x.State.Checked 
                        y.State.HandleChecked <- true)
                x.validateCheckedParents()
        x

    static member single perform name f = (Operation.createNew perform None [] name f).freez()
    
    static member fromOp perform parent op1 =
        match op1 with
        | Single(f,s) -> 
            ( Operation.createNew perform parent [] s (Some f) ).freez()
        | Scenary (ops,s) ->
            let x = Operation.createNew perform parent [] s None
            x.Items <- ops |> List.map( Operation.fromOp perform (Some x) )
            x.freez()
            



