namespace SmartRecipes
open FSharp.Json
    
module Library =
    let first (a, _) = a
    let second (_, b) = b
    let getOk = function
        | Ok a -> a
        | Error e -> e.ToString () |> failwith
    
    let swap f a b = f b a 
        
module Async =
    let id value = async { return value }
    
    let map f asyncTask = async {
        let! v = asyncTask
        return f v
    }
    
module ReaderT =
    open FSharpPlus.Data

    let execute env reader =
        ReaderT.run reader env
        
module ViewElement =
    open Fabulous.DynamicViews

    let withBackButton value (view: ViewElement) =
        view.HasBackButton(value)
        
    let withNavbar value (view: ViewElement) =
        view.HasNavigationBar(value)
        
module Cmd =
    open FSharpPlus.Data
    open Fabulous.Core

    let noneOfReader = ReaderT(fun _ -> Cmd.none)
    
    let ofReader env  =
        ReaderT.execute env >> Cmd.ofAsyncMsg
        
module Lens =
    open FSharpPlus.Control
    open FSharpPlus.Lens
    
    let inline _where p f s =
        let update old = if p old then f old else Return.InvokeOnInstance old
        items update s
        
module Json =
    let serialize = Json.serialize
    let deserialize<'a> =
        let firstToLower (s: string) = s.[0].ToString().ToLowerInvariant() + s.Substring(1)
        Json.deserializeEx<'a> (JsonConfig.create(jsonFieldNaming=firstToLower))