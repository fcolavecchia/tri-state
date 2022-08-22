module tests

open FsToolkit.ErrorHandling
open NUnit.Framework

open TriState.Validation3
open TriState.Validation4

type MyError =
    | MyError of string
    member this.Value =
        let (MyError v) = this
        v
        
let InvalidRangeError min max value =
    MyError $"{value} not in valid range ({min},{max})"
    
type MyInt =
    | MyInt of int
    member this.Value =
        let (MyInt v) = this
        v
        
    static member Create v =
        MyInt v
        
    static member isValid  min  max  value =
        value > min &&  value < max
        
    static member TryCreate min max value : Result<MyInt,MyError> =
        match value with
        | value when (MyInt.isValid min max value) -> Result.Ok (MyInt value) 
        | _ -> Result.Error (InvalidRangeError min max value)
        
    

let max =  5
let min =  0 

        
type MyValidatedInt = Validation3<MyInt,MyError>  

type MyValidated4Int = Validation4<MyInt,MyError>  


let tryCreateMyValidatedInt min max = 
    Validation3.create (MyInt.TryCreate min max >> Validation.ofResult ) MyInt
    
let tryCreateMyValidated4Int min max = 
    create (MyInt.TryCreate min max) MyInt     

[<SetUp>]
let Setup () =
    ()

[<Test>]
let ``Valid int`` () =
    
    let b = 3
    
    let actual = tryCreateMyValidatedInt min max b 
    let expected : MyValidatedInt = Validation3.Valid (MyInt b)
   
    Assert.AreEqual (expected, actual)

[<Test>]
let ``Invalid int`` () =
    
    let b = 6
    
    let actual = tryCreateMyValidatedInt min max b 
    let expected : MyValidatedInt = Validation3.Warning (MyInt b, [InvalidRangeError min max b])
   
    Assert.AreEqual (expected, actual)
    
[<Test>]
let ``Valid int, abbr version`` () =
    
    let b = 3
    
    let actual : MyValidated4Int = tryCreateMyValidated4Int min max b 
    let expected : MyValidated4Int = Some (Ok (MyInt b))
   
    Assert.AreEqual (expected, actual)

[<Test>]
let ``Invalid int, abbr version`` () =
    
    let b = 6
    
    let actual : MyValidated4Int = tryCreateMyValidated4Int min max b 
    let expected : MyValidated4Int = Some (Error (MyInt b, [InvalidRangeError min max b]))
   
    Assert.AreEqual (expected, actual)    