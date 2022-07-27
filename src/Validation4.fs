namespace TriState

open FsToolkit.ErrorHandling

type Validation4<'a,'e> = Option<<'a,'a * 'e list>> 


[<RequireQualifiedAccess>]
module Validation4 =  
    let valueToString  (rawToString: 'a -> string) (defaultValue: string) (x: Validation4<'a, 'e>) : string =
        match x with
        | Some (Ok a) ->
                    printfn $"valueToString: {a} -> {rawToString a}"
                    rawToString a
        | Some (Error (a,_)) -> rawToString a
        | None -> defaultValue    
    
    let map (f: 'a -> 'b) (x: Validation4<'a, 'e>) : Validation4<'b, 'e> =
        match x with
        | Some (Ok a) -> Some (Ok (f a))
        | Some (Error (a,eList)) -> Some (Error (f a, eList))
        | None -> None 
 
    let mapError f (x: Validation4<_, _>) : Validation4<_, _> = //x |> Result.mapError (List.map f)
        match x with
        | Some (Ok a) -> Some (Ok a)
        | Some (Error (a, eList)) -> Some (Error (a, eList |> List.map f))
        | None -> None 

    
    let mapErrors f (x: Validation4<_, _>) : Validation4<_, _> = 
        match x with
        | Some (Ok a) -> Some (Ok a)
        | Some (Error (a, eList)) -> Some (Error (a, f eList))
        | None -> None 

        
    let createValidationF  ( tryCreate: 'b -> Validation<'a,'e>)
            (rawCreate: 'b ->'a)  (rawData:'b) : Validation4<'a,'e> =        
        let resultData = tryCreate rawData
        match resultData with
            | Ok a -> Some (Ok a)
            | Error eList -> Some (Error (rawCreate rawData, eList))
            
            
    let createValidationFWithDefault  ( validate: 'b -> Validation<'a,'e>)
            (defaultOutValue: 'a) (rawData:'b option)  : Validation4<'a,'e> =        
        let resultData =  rawData |> Option.map validate
        match resultData with
            | Some (Ok a) -> Some (Ok a)
            | Some (Error eList) -> Some (Error (defaultOutValue, eList))
            | None -> None
                        
    
    let ofResult (x: Result<'a,'a * 'e list>) : Validation4<'a, 'e> =
        match x with
        | Ok a -> Some (Ok a)
        | Error (a,eList) -> Some (Error (a, eList))
             
    
    let getErrors (x: Validation4<_, _>) =
        match x with
        | Some (Ok _) -> None 
        | Some (Error (_, eList)) -> Some eList
        | None -> None  


    let getValid (x: Validation4<_, _>) =
        match x with
        | Some (Ok a) -> Some a 
        | Some (Error _) -> None
        | None -> None 

    
    let getInvalid (x: Validation4<_, _>) =
        match x with
        | Some (Ok a) -> None 
        | Some (Error (a,_)) -> Some a
        | None -> None  

        
    let getValue  defaultValue (x: Validation4<_, _>)  =
        match x with
        | Some (Ok a) -> a 
        | Some (Error (a,_)) -> a
        | None -> defaultValue
        
        
    let toValidation  (x: Validation4<_, _>) : Validation<_,_> =
        match x with
        | Some (Ok a) -> Validation.Ok a 
        | Some (Error (a,elist)) ->
            match elist with 
                | [] -> Validation.Ok a
                | warnings -> Validation.Error warnings
        | None -> Validation.Error ["None to validate"]        
        
    let mapResult f (x: Validation4<_, _>) : Validation4<_, _> =
        match x with
        | Some (Ok a) ->
            let res = f a
            match res with
                | Ok b -> Some (Ok b)
                | Error e -> Some (Error (a, e))
        | Some (Error (a,eList)) ->
            let res = f a
            match res with
                | Ok b -> Some (Error (b, eList) )
                | Error e -> Some (Error (a, e @ eList ))
        | None -> None 