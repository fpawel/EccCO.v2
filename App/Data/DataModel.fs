module DataModel

open System
open System.ComponentModel
open System.Collections.ObjectModel

open PropertyChanged

type Id = string

module CalculateTermo  =

    type Method = AppSets.CalculateTermoMethod


    [<PropertyChanged.ImplementPropertyChanged>]
    type Item = 
        {   mutable T : decimal
            mutable I : decimal option
            mutable K : decimal option }
        static member createNew() = 
            {   T = 0m
                I = None
                K = None }
        static member create1 t i k = 
            {   T = t
                I = i
                K = k }

    [<PropertyChanged.ImplementPropertyChanged>]
    type ProductTag = 
        {   mutable Items : Item list
            mutable IfonN : int
            mutable KchN : int 
            mutable IsCustom : bool }
        static member createNew() = 
            {   Items = []
                IfonN = 0
                KchN = 0 
                IsCustom = false}


let createNewId() = getUniqueKey 12
let EEPROM_SIZE = 0x800
let createFlashArray() = 
    Array.create EEPROM_SIZE 0xffuy

[<PropertyChanged.ImplementPropertyChanged>]
type Product = 
    {   Id : Id 
        N : int
        mutable ProductType : string
        mutable IsChecked : bool
        mutable IsReportIncluded : bool
        mutable Flash : byte []        
        mutable Ifon : decimal option 
        mutable Isns : decimal option
        mutable I13 : decimal option
        mutable I24 : decimal option
        mutable I35 : decimal option
        mutable I26 : decimal option
        mutable I17 : decimal option
        mutable In : decimal option
        mutable If_20 : decimal option
        mutable Is_20 : decimal option
        mutable If50 : decimal option
        mutable Is50 : decimal option
        mutable Serial : int option
        mutable IsCustomTermo : bool 
        mutable CustomTermo : CalculateTermo.Item list}   
    static member createNew n = 
        {   Id = createNewId()
            ProductType  = ""
            IsChecked = true  
            IsReportIncluded = true
            Ifon = None 
            Isns = None
            I13 = None
            I24 = None
            I35 = None
            I26 = None
            I17 = None
            In = None
            If_20 = None
            Is_20 = None
            If50 = None
            Is50 = None
            Serial = None
            Flash =  Array.create EEPROM_SIZE 0xFFuy
            N = n
            IsCustomTermo = false
            CustomTermo = [] }

[<PropertyChanged.ImplementPropertyChanged>]
type Batch =
    {   Id : Id
        Date : DateTime
        mutable ProductsSerials : int list
        mutable Name : string
        mutable ProductType : string
        mutable PGS1 : decimal
        mutable PGS2 : decimal
        mutable PGS3 : decimal
        Products : Product list }
    static member createNew () = 
        let b = 
            {   Id = createNewId()
                ProductsSerials = [for _ in 0..63 -> 0]
                Date=DateTime.Now
                ProductType="035"
                PGS1=0m
                PGS2=90m
                PGS3=200m
                Name = "Партия1" 
                Products = List.init 64 Product.createNew }
        for p in b.Products do
             subscribePropertyChanged p <| fun e ->
                if e.PropertyName="Serial" then
                    b.ProductsSerials <- Batch.getProductsSerials b.Products
        b

    static member getProductsSerials = 
        List.map( function 
            | {Serial = Some serial } -> serial 
            | _ -> 0)

[<PropertyChanged.ImplementPropertyChanged>]
type BatchInfo = 
    {   Id : Id
        Date : DateTime
        ProductsSerials : int list
        mutable Name : string
        mutable ProductType : string }

    static member creteNew (x : Batch) = 
        {   ProductType = x.ProductType
            Id = x.Id
            Date = x.Date
            Name = x.Name 
            ProductsSerials = x.Products  |> List.map( function 
                | {Serial = Some serial } -> serial 
                | _ -> 0) }

