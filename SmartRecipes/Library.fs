namespace SmartRecipes

module Library =
    let first (a, _) = a
    let second (_, b) = b
    let getOk = function
        | Ok a -> a
        | Error e -> e.ToString () |> failwith
        
module Async =
    let id value = async { return value } 