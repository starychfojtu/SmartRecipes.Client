namespace SmartRecipes

module Library =
    let first (a, _) = a
    let second (_, b) = b
    let getOk = function
        | Ok a -> a
        | Error e -> e.ToString () |> failwith
    
    let swap f a b = f b a 
        
module Async =
    let id value = async { return value }
    
module ReaderT =
    open FSharpPlus.Data

    let execute env reader =
        ReaderT.run reader env
        
module Cmd =
    open FSharpPlus.Data
    open Fabulous.Core

    let noneOfReader = ReaderT(fun _ -> Cmd.none)
    
    let ofReader env  =
        ReaderT.execute env >> Cmd.ofAsyncMsg