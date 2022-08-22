namespace TriState

open FsToolkit.ErrorHandling

module Validation3 =  

    [<RequireQualifiedAccess>]
    type Validation3<'T,'TWarn> =  
        | Valid of 'T
        | Warning of 'T * 'TWarn list  
        | Nothing 
    with
        override this.ToString() : string =
            match this with
            | Valid a ->  a.ToString()
            | Warning (a, warn) ->
                let warnings = warn
                                |> List.fold (fun l w -> l + w.ToString()) ""
                
                a.ToString() + $"Warning: {warnings} "
            | Nothing -> "Nothing"
            
        static member create ( tryCreate: 'U -> Validation<'T,'TWarn>)
            (rawCreate: 'U ->'T)  (rawData:'U) =        
            let resultData = tryCreate rawData
            match resultData with
                | Ok a -> Valid a
                | Error warn -> Warning (rawCreate rawData, warn)
                
            
        static member createWithDefault  ( tryCreate: 'U -> Validation<'T,'TWarn>)
            (defaultOutValue: 'T) (rawData:'U option)  =        
            let resultData =  rawData |> Option.map tryCreate
            match resultData with
                | Some (Ok a) -> Valid a
                | Some (Error warn) -> Warning (defaultOutValue, warn)
                | None -> Nothing
          
                
        
        static member map f x : Validation3<_, _> =
            match x with
            | Valid a -> Valid (f a)
            | Warning (a, warn) -> Warning (f a, warn)
            | Nothing -> Nothing 
        
        static member mapError f x = 
            match x with
            | Valid a -> Valid a
            | Warning (a, warn) -> Warning (a, f warn)
            | Nothing -> Nothing 
        
          
        
        static member ofResult (x: Result<'T,'T * 'TWarn list>)  =
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
            
        static member bind (f: 'T -> Validation3<'U,'TWarn> ) (x: Validation3<'T,'TWarn>) : Validation3<'U,'TWarn> = 
            match x with
            | Valid a ->
                let res = f a
                match res with
                    | Valid b -> Valid b
                    | Warning (b,e) -> Warning (b,e)
                    | Nothing -> Nothing 
            | Warning (a, warn) ->
                let res = f a
                match res with
                    | Valid b -> Valid b
                    | Warning (b,e) -> Warning (b,warn @ e)
                    | Nothing -> Nothing       
            | Nothing -> Nothing 
                    
                
                       
            