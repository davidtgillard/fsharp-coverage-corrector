module FSharpCoverageCorrector.Example

open System
open System.Threading
open System.Threading.Tasks

type MyDU =
  | One of bool
  | Two of int
  
  
type BiggerDU =
  | A
  | B 
  | C
  | D
  | E
  | F

type MyClass(value: int) =
  let mutable value_ = value
  
  let myArray_  = [| "A", "B", "C" |]
  let syncRoot_ = obj() 
  member this.Value = if value_ < 5 then value_ else 5
 
  member this.SetValue(value) =
    lock (syncRoot_) (fun () ->
      value_ <- value
    )
    
  type InnerClass() =
    member this.InnerValue = 5

let lockedFun(y: MyDU) =
  let o = obj()
  lock o (fun _ ->
    match y with
    | One _ -> ()
    | _ -> ()
    let o2 = obj()
    lock o2 (fun _ ->
      Console.WriteLine("CRAP"))
    
    ())
  
let private lockWrapper o fn = lock o fn

let wrappedLockFun(y: MyDU) =
  let o = obj()
  lockWrapper o (fun _ ->
    let x = 2
    match y with
    | One _ -> ()
    | _ -> ()
    ())
  
let sleep (interval: TimeSpan) = async { Thread.Sleep interval }
  
let regularMatch(y: MyDU) =
  match y with
  | One _ -> ()
  | _ -> ()

let runAsync(y: MyDU) =
  async {
    do! sleep (TimeSpan.FromSeconds(0.5))
    let o = obj()
    lock o (fun () ->
      match y with
      | One _ -> ()
      | _ -> ()
      let t = 1
      if t = 1 then
        Console.WriteLine("Hi")
      else if t = 2 then
        Console.WriteLine("There")
    )
    return 2
  }
  
let runTask(y: MyDU) =
  task {
    let! x = Task.Delay(TimeSpan.FromSeconds(0.5))
    match y with
    | One _ -> ()
    | Two _ -> ()
    | _ -> ()
    let t = 1
    if t = 1 then
      Console.WriteLine("Hi")
    elif t = 2 then
      Console.WriteLine("There")
    elif t = 3 then
      Console.WriteLine("You")
    else
      Console.WriteLine("Nope")
    return 2
  }
 
let mySeq() =
  seq {
    yield 1
    yield 2
    yield! [3; 4]
  }
  
let anonStruct() =
  let x = {| A="string"; B=true |}
  Console.WriteLine(x.ToString())
  x
  
let arraySlice() =
  let x = [| 1..1000 |]
  x[100..200]
  
let stringSlice() =
  let s = "supercalifragilisticexpialidocious"
  s[5..]
  
let highComplexity() =
  let x = 5
  if x = 1 then
    ()
  else if x = 2 then
    ()
  else if x = 2 then
    ()
  else if x = 3 then
    ()
  else if x = 4 then
    ()
  else if x = 5 then
    ()
  else
    ()
    
let whileFunc() =
  let mutable i = 0
  while (i < 10) do
    i <- i+1
  
let bigMatch() =
  let a = BiggerDU.A
  match a with
  | A -> ()
  | B -> ()
  | C -> ()
  | D -> ()
  
let bigMatchOneLiner() =
  let a = BiggerDU.A
  match a with | A -> () | B -> () | C -> () | D -> () | E -> () | F -> ()
  
let tryFunc() =
  try
    failwith "uh oh!"
  with
  | :? ArgumentException ->
    ()
  | _ ->
    ()