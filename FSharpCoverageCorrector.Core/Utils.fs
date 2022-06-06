/// Internal utilities.
module internal FSharpCoverageCorrector.Core.Utils

/// replaces all items in a list that satisfy a conditional.
let replace (``replace?``: 't -> 't -> bool) (newItem: 't) (l: 't list)  =
  l |> List.map (fun (elem: 't) -> if ``replace?`` newItem elem then
                                     newItem
                                   else
                                     elem)