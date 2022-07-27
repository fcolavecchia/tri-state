


[<RequireQualifiedAccess>]
module ValidationF =  
    let valueToString  (rawToString: 'a -> string) (defaultValue: string) (x: ValidationF<'a, 'e>) : string =
        match x with
        | Valid a ->
                    printfn $"valueToString: {a} -> {rawToString a}"
                    rawToString a
        | Warning (a,_) -> rawToString a
        | Nothing -> defaultValue 
 
    
    
    let map f (x: ValidationF<_, _>) : ValidationF<_, _> =
        match x with
        | Valid a -> Valid (f a)
        | Warning (a,eList) -> Warning (f a, eList)
        | Nothing -> Nothing 
 
    let mapError f (x: ValidationF<_, _>) : ValidationF<_, _> = //x |> Result.mapError (List.map f)
        match x with
        | Valid a -> Valid a
        | Warning (a, eList) -> Warning (a, eList |> List.map f)
        | Nothing -> Nothing 

    
    let mapErrors f (x: ValidationF<_, _>) : ValidationF<_, _> = 
        match x with
        | Valid a -> Valid a
        | Warning (a, eList) -> Warning (a, f eList)
        | Nothing -> Nothing 

        
    let createValidationF  ( tryCreate: 'b -> Validation<'a,'e>)
            (rawCreate: 'b ->'a)  (rawData:'b) : ValidationF<'a,'e> =        
        let resultData = tryCreate rawData
        match resultData with
            | Ok a -> Valid a
            | Error eList -> Warning (rawCreate rawData, eList)
            
            
    let createValidationFWithDefault  ( validate: 'b -> Validation<'a,'e>)
            (defaultOutValue: 'a) (rawData:'b option)  : ValidationF<'a,'e> =        
        let resultData =  rawData |> Option.map validate
        match resultData with
            | Some (Ok a) -> Valid a
            | Some (Error eList) -> Warning (defaultOutValue, eList)
            | None -> Nothing
            
            
            
            
    
    let ofResult (x: Result<'a,'a * 'e list>) : ValidationF<'a, 'e> =
        match x with
        | Ok a -> Valid a
        | Error (a,eList) -> Warning (a, eList)
             
    
    let getErrors (x: ValidationF<_, _>) =
        match x with
        | Valid _ -> None 
        | Warning (_, eList) -> Some eList
        | Nothing -> None  


    let getValid (x: ValidationF<_, _>) =
        match x with
        | Valid a -> Some a 
        | Warning _ -> None
        | Nothing -> None 

    
    let getInvalid (x: ValidationF<_, _>) =
        match x with
        | Valid a -> None 
        | Warning (a,_) -> Some a
        | Nothing -> None  

        
    let getValue  defaultValue (x: ValidationF<_, _>)  =
        match x with
        | Valid a -> a 
        | Warning (a,_) -> a
        | Nothing -> defaultValue
        
        
    let toValidation  (x: ValidationF<_, _>)  =
        match x with
        | Valid a -> Validation.Ok a 
        | Warning (a,elist) ->
            match elist with 
                | [] -> Validation.Ok a
                | warnings -> Validation.Error warnings
        | Nothing -> Validation.Error [GeneralError.ValidationError "Nothing to validate"]        
        
    let mapResult f (x: ValidationF<_, _>) : ValidationF<_, _> =
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
                
            
                   
        