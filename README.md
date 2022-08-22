# tri-state

A type to model some kind of ternary states. 

In our domain we need to deal with some data that 
- Can be a valid one
- Does not exists or 
- Can be invalid, that is, the value of the data does not pass validations,
but we are required to emit a warning to the user. 

So this cannot be modeled with an `Option` type, because we need to distinguish
between the valid and invalid (with warning) data. Nor it can be represented
by a `Result` type, because even in the case of the invalid data, we need to
report a warning.

One of the use cases is an integer that has to be between values
`min` and `max`. Therefore, the validation condition is  
`a > min && a < max`. In any other case, we emit an `InvalidRange` warning.
Also, it is possible that the integer does not exist at all.

A possible solution is to represent this data as
```f#
type Validation3<'T,'TWarn> =  
        | Valid of 'T
        | Warning of 'T * 'TWarn list  
        | Nothing 
```
In this way, we have all the cases we need.
However, a careful observation would show that they are still not
mutually exclusive: It would be possible to have a `Warning` state with
an empty `'TWarn` list of warnings, which would be equivalent to a `Valid`
state. 






