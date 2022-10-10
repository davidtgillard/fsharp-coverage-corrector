/// Internal utilities.
module internal FSharpCoverageCorrector.Core.Utils

open System

/// replaces all items in a list that satisfy a conditional.
let replace (``replace?``: 't -> 't -> bool) (newItem: 't) (l: 't list)  =
  l |> List.map (fun (elem: 't) -> if ``replace?`` newItem elem then
                                     newItem
                                   else
                                     elem)
  
/// Given a list of results, filters the oks and unwraps them
let unwrapOks (results : Result<'o, 'e> list) =
  results |> List.filter (fun r -> match r with
                                   | Ok _ -> true
                                   | Error _ -> false
                         )
          |> List.map (fun r -> match r with
                                | Ok x -> x
                                | Error _ -> raise (InvalidOperationException("Error instances should all be filtered out prior to this stage"))
                      )
  
/// Given a list of errors, filters the oks and unwraps them
let unwrapErrors (results : Result<'o, 'e> list) =
  results |> List.filter (fun r -> match r with
                                   | Ok _ -> false
                                   | Error _ -> true
                         )
          |> List.map (fun r -> match r with
                                | Ok _ -> raise (InvalidOperationException("Ok instances should all be filtered out prior to this stage"))
                                | Error x -> x 
                      )
          
/// Given a list of results, partitions them into Oks and Errors, and unwraps each
let unwrapAndPartitionResults (results: Result<'o, 'e> list) =
  let okResults, errorResults =
    results
    |> List.partition (fun r -> match r with
                                | Ok _ -> true
                                | Error _ -> false)
  unwrapOks okResults, unwrapErrors errorResults 
                                    