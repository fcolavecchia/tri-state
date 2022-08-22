namespace TriState


module Validation3 =  

    [<RequireQualifiedAccess>]
    type Validation3<'T,'TWarn> =  
        | Valid of 'T
        | Warning of 'T * 'TWarn 
        | Nothing 
    with
        override this.ToString() : string =
            match this with
            | Valid a ->  a.ToString()
            | Warning (a, warn) ->  a.ToString() + $"Warning: {warn} "
            | Nothing -> "Nothing"      
    
        static member map f x : Validation3<_, _> =
            match x with
            | Valid a -> Valid (f a)
            | Warning (a, warn) -> Warning (f a, warn)
            | Nothing -> Nothing 
 
        static member mapError f x = //x |> Result.mapError (List.map f)
            match x with
            | Valid a -> Valid a
            | Warning (a, warn) -> Warning (a, f warn)
            | Nothing -> Nothing 
        
        static member create ( tryCreate: 'U -> Result<'T,'TWarn>)
            (rawCreate: 'U ->'T)  (rawData:'U) =        
            let resultData = tryCreate rawData
            match resultData with
                | Ok a -> Valid a
                | Error warn -> Warning (rawCreate rawData, warn)            
            
        static member createWithDefault  ( tryCreate: 'U -> Result<'T,'TWarn>)
            (defaultOutValue: 'T) (rawData:'U option)  =        
            let resultData =  rawData |> Option.map tryCreate
            match resultData with
                | Some (Ok a) -> Valid a
                | Some (Error warn) -> Warning (defaultOutValue, warn)
                | None -> Nothing
          
    
        static member ofResult (x: Result<'T,'T * 'TWarn>)  =
            match x with
            | Ok a -> Valid a
            | Error (a, warn) -> Warning (a, warn)
             
    
        static member getErrors x =
            match x with
            | Valid _ -> None 
            | Warning (_, warn) -> Some warn
            | Nothing -> None  


        static member getValid x =
            match x with
            | Valid a -> Some a 
            | Warning _ -> None
            | Nothing -> None 

    
        static member getInvalid x =
            match x with
            | Valid _ -> None 
            | Warning (a,  _) -> Some a
            | Nothing -> None  

        
        static member getValue  defaultValue x  =
            match x with
            | Valid a -> a 
            | Warning (a, _) -> a
            | Nothing -> defaultValue
            
        static member mapResult f x : Validation3<_, _> =
            match x with
            | Valid a ->
                let res = f a
                match res with
                    | Ok b -> Valid b
                    | Error e -> Warning (a, e)
            | Warning (a, warn) ->
                let res = f a
                match res with
                    | Ok b -> Warning (b, warn) 
                    | Error e -> Warning (a,warn )
            | Nothing -> Nothing 
                    
                
                       
            