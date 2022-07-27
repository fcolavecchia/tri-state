namespace TriState

open FsToolkit.ErrorHandling

type Validation3<'a,'e> =  //Result<'a,'a * ('e list)>
    | Valid of 'a
    | Warning of 'a * 'e list
    | Nothing 
    



[<RequireQualifiedAccess>]
module Validation3 =  
    let valueToString  (rawToString: 'a -> string) (defaultValue: string) (x: Validation3<'a, 'e>) : string =
        match x with
        | Valid a ->
                    printfn $"valueToString: {a} -> {rawToString a}"
                    rawToString a
        | Warning (a,_) -> rawToString a
        | Nothing -> defaultValue      
    
    let map f (x: Validation3<_, _>) : Validation3<_, _> =
        match x with
        | Valid a -> Valid (f a)
        | Warning (a,eList) -> Warning (f a, eList)
        | Nothing -> Nothing 
 
    let mapError f (x: Validation3<_, _>) : Validation3<_, _> = //x |> Result.mapError (List.map f)
        match x with
        | Valid a -> Valid a
        | Warning (a, eList) -> Warning (a, eList |> List.map f)
        | Nothing -> Nothing 

    
    let mapErrors f (x: Validation3<_, _>) : Validation3<_, _> = 
        match x with
        | Valid a -> Valid a
        | Warning (a, eList) -> Warning (a, f eList)
        | Nothing -> Nothing 

        
    let createValidationF  ( tryCreate: 'b -> Validation<'a,'e>)
            (rawCreate: 'b ->'a)  (rawData:'b) : Validation3<'a,'e> =        
        let resultData = tryCreate rawData
        match resultData with
            | Ok a -> Valid a
            | Error eList -> Warning (rawCreate rawData, eList)
            
            
    let createValidationFWithDefault  ( validate: 'b -> Validation<'a,'e>)
            (defaultOutValue: 'a) (rawData:'b option)  : Validation3<'a,'e> =        
        let resultData =  rawData |> Option.map validate
        match resultData with
            | Some (Ok a) -> Valid a
            | Some (Error eList) -> Warning (defaultOutValue, eList)
            | None -> Nothing
            
            
            
            
    
    let ofResult (x: Result<'a,'a * 'e list>) : Validation3<'a, 'e> =
        match x with
        | Ok a -> Valid a
        | Error (a,eList) -> Warning (a, eList)
             
    
    let getErrors (x: Validation3<_, _>) =
        match x with
        | Valid _ -> None 
        | Warning (_, eList) -> Some eList
        | Nothing -> None  


    let getValid (x: Validation3<_, _>) =
        match x with
        | Valid a -> Some a 
        | Warning _ -> None
        | Nothing -> None 

    
    let getInvalid (x: Validation3<_, _>) =
        match x with
        | Valid a -> None 
        | Warning (a,_) -> Some a
        | Nothing -> None  

        
    let getValue  defaultValue (x: Validation3<_, _>)  =
        match x with
        | Valid a -> a 
        | Warning (a,_) -> a
        | Nothing -> defaultValue
        
        
    let toValidation  (x: Validation3<_, _>)  =
        match x with
        | Valid a -> Validation.Ok a 
        | Warning (a,elist) ->
            match elist with 
                | [] -> Validation.Ok a
                | warnings -> Validation.Error warnings
        | Nothing -> Validation.Error ["Nothing to validate"]        
        
    let mapResult f (x: Validation3<_, _>) : Validation3<_, _> =
        match x with
        | Valid a ->
            let res = f a
            match res with
                | Ok b -> Valid b
                | Error e -> Warning (a, e)
        | Warning (a,eList) ->
            let res = f a
            match res with
                | Ok b -> Warning (b, eList) 
                | Error e -> Warning (a, e @ eList )
        | Nothing -> Nothing 
                
            
                   
        