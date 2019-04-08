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
        Api.sendSearchFoodstuffsRequest accessToken term |> Async.map (fun r -> NewResults r.Foodstuffs)
    
    let update model = function
        | TermChanged term ->
            { model with Term = term }, search model.AccessToken term |> Cmd.ofAsyncMsg
        | NewResults results ->
            { model with Results = results }, Cmd.none
        | FoodstuffSelected foodstuff ->
            failwith "not implemented"
            
    let searchBar dispatch =
        View.SearchBar(textChanged = (fun args -> TermChanged args.NewTextValue |> dispatch))
        
    let resultTableItem (foodstuff: Foodstuff) =
        View.Label foodstuff.Name
        
    let resultTable results =
        View.ListView(
            rowHeight = 64,
            items = Seq.map resultTableItem results
        )
        
            
    let view dispatch model =
        View.ContentPage(
            content = View.StackLayout(
                children = [
                    yield searchBar dispatch
                    yield resultTable model.Results
                ]                 
            )
        )

