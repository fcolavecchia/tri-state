module tests

open FsToolkit.ErrorHandling
open NUnit.Framework

open TriState 

type MyError =
    | MyError of string
    member this.Value =
        let (MyError v) = this
        v
    
type MyInt =
    | MyInt of int
    member this.Value =
        let (MyInt v) = this
        v
    static member Create v =
        MyInt v
        
    static member isValid  min  max  value =
        value > min &&  value < max
        
    static member TryCreate min max value : Validation<MyInt,MyError> =
        match value with
        | value when (MyInt.isValid min max value) -> Validation.Ok (MyInt value) 
        | _ -> Validation.Error [MyError $"{value} not in valid range ({min},{max})"]
        
    

let max =  5
let min =  0 

        
type MyValidatedInt = Validation3<MyInt,MyError>  


[<SetUp>]
let Setup () =
    ()

[<Test>]
let ``Valid int`` () =
    
    let b = 3
    
    let actual = Validation3.createValidationF (MyInt.TryCreate min max) MyInt b 
    let expected : MyValidatedInt = Valid (MyInt b)
   
    Assert.AreEqual (expected, actual)
