namespace SmartRecipes

module SearchFoodstuffPage =
    open Domain
    open Fabulous.Core
    open Fabulous.DynamicViews
    
    type Model = {
        term: string
        results: Foodstuff seq
    }
    
    type Message =
        | TermChanged of string
        | NewResults of Foodstuff seq
        | FoodstuffSelected of Foodstuff
        
    let initModel = {
        term = ""
        results = Seq.empty
    }

    let search term =
        async { return Seq.empty }
    
    let update model = function
        | TermChanged term ->
            { model with term = term }, search term |> Cmd.ofAsyncMsg
        | NewResults results ->
            { model with results = results }, Cmd.none
        | FoodstuffSelected foodstuff ->
            failwith "not implemented"
            
    let view dispatch model =
        View.ContentPage()

