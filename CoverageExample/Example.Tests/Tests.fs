module FSharpCoverageCorrector.Example.Tests

open Xunit
open FSharpCoverageCorrector.Example

type MyDUs () =    
    let values : seq<obj[]>  =
        seq { 
        yield [| MyDU.One true |] 
        yield [| MyDU.Two 5 |] // in recent versions of F#, `yield` is optional in seq too
    }
    interface seq<obj[]> with
        member this.GetEnumerator () = values.GetEnumerator()
        member this.GetEnumerator () =
            values.GetEnumerator() :> System.Collections.IEnumerator

[<Theory>]
[<ClassData(typeof<MyDUs>)>]
let ``Test lockedFun`` du =
  lockedFun(du)
  Assert.True(true)

[<Theory>]
[<ClassData(typeof<MyDUs>)>]
let ``Test wrappedLockFun`` du =
  wrappedLockFun(du)
  Assert.True(true)
    
[<Theory>]
[<ClassData(typeof<MyDUs>)>]
let ``Test wrappedRunAsync`` (du) =
  let result = runAsync(du) |> Async.RunSynchronously
  Assert.Equal(2, result)
    
[<Theory>]
[<ClassData(typeof<MyDUs>)>]
let ``Test wrappedRunTask`` (du) =
  let result = runTask(du).Result
  Assert.Equal(2, result)
  
[<Theory>]
[<ClassData(typeof<MyDUs>)>]
let ``Test regularMatch`` (du) =
  regularMatch du
  Assert.True(true)
  
[<Theory>]
[<InlineData(true)>]
[<InlineData(false)>]
let ``Test doBang`` (cond) =
  doBang cond |> ignore
  Assert.True(true)
  
[<Fact>]
let ``Test Myclass`` () =
  let c1 = MyClass(1)
  let c2 = MyClass(6)
  Assert.Equal(c1.Value, 1)
  Assert.Equal(c2.Value, 5)
  
[<Fact>]
let ``Test MyStruct`` () =
  let myStruct1 = { IntVal = 4; StringVal = "hello" }
  let myStruct2 = { IntVal = 4; StringVal = "hello" }
  Assert.Equal(myStruct1, myStruct2)
 
[<Fact>]
let TestSeq () =
  mySeq() |> Seq.iter (fun _ -> ())
  Assert.True(true)
  
[<Fact>]
let ``Test anonStruct`` () =
  Assert.NotNull(anonStruct())
  
[<Fact>]
let ``Test stringSlice`` () =
  stringSlice() |> ignore
  Assert.True(true)
  
[<Fact>]
let ``Test arraySlice`` () =
  arraySlice() |> ignore
  Assert.True(true)