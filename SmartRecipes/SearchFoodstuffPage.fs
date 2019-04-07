namespace SmartRecipes

module SearchFoodstuffPage =
    open Domain
    open FSharpx.Control
    open Fabulous.Core
    open Fabulous.DynamicViews
    
    type Model = {
        Term: string
        Results: Foodstuff seq
        AccessToken: AccessToken
    }
    
    type Message =
        | TermChanged of string
        | NewResults of Foodstuff seq
        | FoodstuffSelected of Foodstuff
        
    let initModel accessToken = {
        Term = ""
        Results = Seq.empty
        AccessToken = accessToken
    }

    let search accessToken term =
        Api.sendSearchFoodstuffsRequest accessToken term |> Async.map (fun r -> r.Foodstuffs)
    
    let update model = function
        | TermChanged term ->
            { model with Term = term }, search model.AccessToken term |> Cmd.ofAsyncMsg
        | NewResults results ->
            { model with Results = results }, Cmd.none
        | FoodstuffSelected foodstuff ->
            failwith "not implemented"
            
    let view dispatch model =
        View.ContentPage()

