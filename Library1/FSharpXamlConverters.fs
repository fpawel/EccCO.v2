namespace FSharpXamlConverters

open System
open System.Windows.Data
open System.Windows
open System.Windows.Media

open NLog

type LogLevelForegroundConverter () = 
    interface  IValueConverter with
        override __.Convert(x, _, _, _) =
            let x = x :?> NLog.LogLevel  
            let x = 
                if x=LogLevel.Error || x=LogLevel.Fatal then Colors.Yellow
                elif x=LogLevel.Warn then Colors.LightCoral
                elif x=LogLevel.Info then Colors.SkyBlue else Colors.LightGray
            new SolidColorBrush(x) |> box
        override __.ConvertBack(_, _, _, _) = 
            raise (new NotImplementedException())

type FSharpDecimalOptionConverter () = 
    interface  IValueConverter with
        override __.Convert(x, _, _, _) =            
            match x :?> decimal option with
            | Some x -> box x
            | _ -> null
        override __.ConvertBack(x, _, _, _) = 
            let x = x :?> string
            if x = "" then None else
                let (~%%) x = 
                    Decimal.TryParse x
                let (|M|_|) (b,x:decimal) = if b then Some x else None
                let (<->) (z:string) (y:string) = %% Text.RegularExpressions.Regex.Replace(x,z,y)
                match %% x, "\\." <-> ",", "," <-> "\\." with
                | M x, _, _ -> Some x
                | _, M x, _ -> Some x
                | _, _, M x -> Some x
                | _ -> None            
            |> box


type FSharpIntOptionConverter () = 
    interface  IValueConverter with
        override __.Convert(x, _, _, _) =            
            match x :?> int option with
            | Some x -> box x
            | _ -> null
        override __.ConvertBack(x, _, _, _) = 
            let x = x :?> string
            if x = "" then None else
                let b,v = Int32.TryParse x
                if b then Some v else None
            |> box

type FSharpDateTimeOptionConverter () = 
    interface  IValueConverter with
        override __.Convert(x, _, _, _) =            
            match x :?> DateTime option with
            | Some x -> box x
            | _ -> null
        override __.ConvertBack(x, _, _, _) = 
           raise (NotImplementedException())
            


