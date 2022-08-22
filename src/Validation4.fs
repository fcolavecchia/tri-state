namespace TriState


open FsToolkit.ErrorHandling


module Validation4 =  
    
    // Since this type is a type abbreviation, it cannot be augmented
    // with member functions.
    type Validation4<'T,'TWarn> = Option<Result<'T,'T * 'TWarn list>> 

    
    let ToString x : string =
            match x with
            | Some (Ok a) ->  a.ToString()
            | Some (Error (a, warn)) ->  a.ToString() + $"Warning: {warn} "
            | None -> "Nothing"      
    
    let map (f: 'T -> 'b) (x: Validation4<'T, 'TWarn>) : Validation4<'b, 'TWarn> = 
        match x with        
        | Some (Ok a) -> Some (Ok (f a))
        | Some (Error (a, warn)) -> Some (Error (f a, warn))
        | None -> None 
 
    let mapError f x = 
        match x with
        | Some (Ok a) -> Some (Ok a)
        | Some (Error (a, warn)) -> Some (Error (a, warn |> List.map f))
        | None -> None 

    
    let mapErrors f x= 
        match x with
        | Some (Ok a) -> Some (Ok a)
        | Some (Error (a, warn)) -> Some (Error (a, f warn))
        | None -> None 

        
    let create  ( tryCreate: 'b -> Result<'T,'TWarn>)
            (rawCreate: 'b ->'T)  (rawData:'b) : Validation4<'T,'TWarn> =        
        let resultData = tryCreate rawData
        match resultData with
            | Ok a -> Some (Ok a)
            | Error warn -> Some (Error (rawCreate rawData, [warn]))
            
            
    let createWithDefault  ( validate: 'b -> Result<'T,'TWarn>)
            (defaultOutValue: 'T) (rawData:'b option)  : Validation4<'T,'TWarn> =        
        let resultData =  rawData |> Option.map validate
        match resultData with
            | Some (Ok a) -> Some (Ok a)
            | Some (Error warn) -> Some (Error (defaultOutValue, [warn]))
            | None -> None
                        
    
    let ofResult (x: Result<'T,'T * 'TWarn >) : Validation4<'T, 'TWarn> =
        match x with
        | Ok a -> Some (Ok a)
        | Error (a,warn) -> Some (Error (a, [warn]))
             
    
    let getErrors x =
        match x with
        | Some (Ok _) -> None 
        | Some (Error (_, warn)) -> Some warn
        | None -> None  


    let getValid x =
        match x with
        | Some (Ok a) -> Some a 
        | Some (Error _) -> None
        | None -> None 

    
    let getInvalid x =
        match x with
        | Some (Ok _) -> None 
        | Some (Error (a,_)) -> Some a
        | None -> None  

        
    let getValue  defaultValue x  =
        match x with
        | Some (Ok a) -> a 
        | Some (Error (a,_)) -> a
        | None -> defaultValue
           
        
    let mapResult f x =
        match x with
        | Some (Ok a) ->
            let res = f a
            match res with
                | Ok b -> Some (Ok b)
                | Error e -> Some (Error (a, e))
        | Some (Error (a,warn)) ->
            let res = f a
            match res with
                | Ok b -> Some (Error (b, warn) )
                | Error e -> Some (Error (a, e))
        | None -> None
        
    
       