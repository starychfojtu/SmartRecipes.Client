namespace SmartRecipes

module SearchFoodstuffPage =
    open AppEnvironment
    open Domain
    open FSharpPlus
    open FSharpPlus.Data
    open Fabulous.Core
    open Fabulous.DynamicViews
    open Xamarin.Forms
    
    type Model = {
        Term: string
        Results: Foodstuff seq
    }
    
    type Message =
        | TermChanged of string
        | NewResults of Foodstuff seq
        | TryAddFoodstuff of Foodstuff
        
    let initModel = {
        Term = ""
        Results = Seq.empty
    }

    let private searchFoodstuff term = ReaderT(fun env -> 
        env.Api.SearchFoodstuffs { Term = term })
    
    let private search term =
        searchFoodstuff term |> ReaderT.map (fun r -> NewResults r.Foodstuffs)
    
    type UpdateResult = 
        | ModelUpdated of Model * Cmd<Message>
        | FoodstuffSelected of Foodstuff
            
    let update model message env =
        match message with
        | TermChanged term ->
            ModelUpdated ({ model with Term = term }, search term |> Cmd.ofReader env)
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
            orientation = StackOrientation.Horizontal,
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
                            text = amountToString foodstuff.BaseAmount
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
                            text = "Add",
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
            
    let view dispatch model ignoredFoodstuffs =
        let foodstuffs = Seq.except ignoredFoodstuffs model.Results
        View.ContentPage(
            content = View.StackLayout(
                children = [
                    yield searchBar dispatch
                    yield resultTable dispatch foodstuffs
                ]                 
            )
        )

