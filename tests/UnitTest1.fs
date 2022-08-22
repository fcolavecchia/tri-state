module tests

open NUnit.Framework

open TriState.Validation3

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

let tryCreateMyValidatedInt min max = 
    Validation3.create (MyInt.TryCreate min max) MyInt 

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
    let expected : MyValidatedInt = Validation3.Warning (MyInt b, InvalidRangeError min max b)
   
    Assert.AreEqual (expected, actual)