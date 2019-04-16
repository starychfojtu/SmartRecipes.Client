namespace SmartRecipes

module SearchFoodstuffPage =
    open Domain
    open FSharpx.Control
    open Fabulous.Core
    open Fabulous.DynamicViews
    open Xamarin.Forms
    
    type Model = {
        Term: string
        Results: Foodstuff seq
        AccessToken: AccessToken
        Api: Api.SmartRecipesApi
    }
    
    type Message =
        | TermChanged of string
        | NewResults of Foodstuff seq
        | TryAddFoodstuff of Foodstuff
        
    let initModel accessToken api = {
        Term = ""
        Results = Seq.empty
        AccessToken = accessToken
        Api = api
    }

    let private search (api: Api.SmartRecipesApi) accessToken term =
        api.SearchFoodstuffs { Term = term; AccessToken = accessToken } |> Async.map (fun r -> NewResults r.Foodstuffs)
    
    type UpdateResult = 
        | ModelUpdated of Model * Cmd<Message>
        | FoodstuffSelected of Foodstuff
            
    let update model = function
        | TermChanged term ->
            ModelUpdated ({ model with Term = term }, search model.Api model.AccessToken term |> Cmd.ofAsyncMsg)
        | NewResults results ->
            ModelUpdated ({ model with Results = results }, Cmd.none)
        | TryAddFoodstuff foodstuff ->
            FoodstuffSelected foodstuff
            
    let private searchBar dispatch =
        View.SearchBar(textChanged = (fun args -> TermChanged args.NewTextValue |> dispatch))
        
    let private amountToString amount =
        amount.Value.ToString () + " " + amount.Unit.ToString ()
        
    let private resultTableItem dispatch (foodstuff: Foodstuff) =
        View.StackLayout(
            children = [
                yield View.StackLayout(
                    verticalOptions = LayoutOptions.CenterAndExpand,
                    children = [
                        yield View.Label(
                            horizontalOptions = LayoutOptions.Start,
                            verticalOptions = LayoutOptions.Center,
                            text = foodstuff.Name
                        )
                        yield View.Label(
                            horizontalOptions = LayoutOptions.Start,
                            verticalOptions = LayoutOptions.Center,
                            text = amountToString foodstuff.AmountStep
                        )
                    ]
                )
                yield View.StackLayout(
                    horizontalOptions = LayoutOptions.EndAndExpand,
                    orientation = StackOrientation.Horizontal,
                    children = [
                        yield View.Button(
                            verticalOptions = LayoutOptions.Center,
                            widthRequest = 64.0,
                            heightRequest = 64.0,
                            command = fun () -> TryAddFoodstuff foodstuff |> dispatch
                        )
                    ]
                )
            ]
        )
        
    let private resultTable dispatch results =
        View.ListView(
            rowHeight = 64,
            items = Seq.map (resultTableItem dispatch) results
        )
            
    let view dispatch model =
        View.ContentPage(
            content = View.StackLayout(
                children = [
                    yield searchBar dispatch
                    yield resultTable dispatch model.Results
                ]                 
            )
        )

