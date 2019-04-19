namespace SmartRecipes

[<RequireQualifiedAccess>]
module ShoppingListPage =
    open Api
    open Domain
    open FSharpPlus
    open FSharpPlus.Data
    open FSharpx.Control
    open Fabulous.Core
    open Fabulous.DynamicViews
    open Xamarin.Forms
    
    type Item = {
        Foodstuff: Foodstuff
        Amount: float
    }
    
    type LoadedModel = {
        Items: Item seq
        AccessToken: AccessToken
        AddFoodstuffPage: SearchFoodstuffPage.Model
        ShowAddFoodstuffPage: bool
        Api: SmartRecipesApi
    }
    
    type Model =
        | Loading
        | Loaded of LoadedModel
        
    type Message =
        | PageLoaded of LoadedModel
        | ItemAmountIncreaseRequested of Item
        | ItemAmountIncreased of Item
        | ItemAmountDecreaseRequested of Item
        | ItemAmountDecreased of Item
        | ItemRemoved of Item
        | GoToAddFoodstuffPage
        | GoToRootPage
        | AddFoodstuffPage of SearchFoodstuffPage.Message
        | ShoppingListChanged of Item seq
        
    // Initialization
    
    let initModel = Loading
    
    let private getShoppingList () = ReaderT(fun (api, accessToken) -> 
        api.GetShoppingList { AccessToken = accessToken } |> Async.map (fun r -> r.ShoppingList))
    
    let private getFoodstuffsByIds foodstuffIds = ReaderT(fun (api, accessToken) ->
        api.GetFoodstuffsById { Ids = foodstuffIds; AccessToken = accessToken } |> Async.map (fun r -> r.Foodstuffs))
    
    let private getFoodstuffs (shoppingList: ShoppingList) = monad {
        let foodstuffIds = Seq.map (fun i -> i.FoodstuffId) shoppingList.Items
        let! foodstuffs = getFoodstuffsByIds foodstuffIds
        return Seq.map (fun (f: Foodstuff) -> (f.Id, f)) foodstuffs |> Map.ofSeq
    }
    
    let private toItems foodstuffs =
         Seq.map (fun i -> { Foodstuff = Map.find i.FoodstuffId foodstuffs; Amount = i.Amount })
         
    let private shoppingListToItems shoppingList = monad {
        let! foodstuffs = getFoodstuffs shoppingList 
        return toItems foodstuffs shoppingList.Items
    }
         
    let private getItems () = monad {
        let! shoppingList = getShoppingList ()
        return! shoppingListToItems shoppingList
    }
    
    let private createModel items = ReaderT(fun (api, accessToken) -> async {
        return PageLoaded {
            Items = items
            AccessToken = accessToken
            AddFoodstuffPage = SearchFoodstuffPage.initModel accessToken api
            ShowAddFoodstuffPage = false
            Api = api
        }
    })
    
    let init () = monad {
        let! items = getItems ()
        return! createModel items
    }
    
    // Update
    
    let addFoodstuffToShoppingList id = ReaderT(fun (api, accessToken) ->
        api.AddFoodstuffsToShoppingList { Ids = [| id |]; AccessToken = accessToken })
    
    let tryAddFoodstuff (foodstuff: Foodstuff) = monad {
        let! response = addFoodstuffToShoppingList foodstuff.Id
        let! items = shoppingListToItems response.ShoppingList
        return ShoppingListChanged items
    }
    
    let update model msg =
        match model with
        | Loading ->
            match msg with
            | PageLoaded loadedModel ->
                Loaded loadedModel, Cmd.none
            | _ ->
                failwith "Unhandled message"
        | Loaded m ->
            match msg with
            | ItemAmountIncreaseRequested id ->
                Loaded m, Cmd.none
            | ItemAmountDecreased id ->
                Loaded m, Cmd.none
            | ItemAmountDecreaseRequested id ->
                Loaded m, Cmd.none
            | ItemAmountIncreased id ->
                Loaded m, Cmd.none
            | ItemRemoved id ->
                Loaded m, Cmd.none
            | GoToAddFoodstuffPage ->
                Loaded { m with ShowAddFoodstuffPage = true }, Cmd.none
            | GoToRootPage ->
                Loaded { m with ShowAddFoodstuffPage = false }, Cmd.none
            | AddFoodstuffPage addFoodstuffPageMessage ->
                let result = SearchFoodstuffPage.update m.AddFoodstuffPage addFoodstuffPageMessage
                match result with
                | SearchFoodstuffPage.UpdateResult.ModelUpdated (newModel, cmd) ->
                    Loaded { m with AddFoodstuffPage = newModel }, Cmd.map AddFoodstuffPage cmd
                | SearchFoodstuffPage.UpdateResult.FoodstuffSelected f ->
                    Loaded m,  ReaderT.run (tryAddFoodstuff f) (m.Api, m.AccessToken) |> Cmd.ofAsyncMsg
            | ShoppingListChanged items ->
                Loaded { m with Items = items }, Cmd.none
            | _ -> failwith "Unhandled message"
        
    // View

    let itemView increase decrease item =
        View.StackLayout(
            orientation = StackOrientation.Horizontal, 
            children = [
                yield View.StackLayout(
                    verticalOptions = LayoutOptions.CenterAndExpand,
                    children = [
                        yield View.Label(
                            text = item.Foodstuff.Name,
                            horizontalOptions = LayoutOptions.Start,
                            verticalOptions = LayoutOptions.Center
                        )
                        yield View.Label(
                            text = item.Amount.ToString() + " " + item.Foodstuff.AmountStep.Unit.ToString(),
                            verticalOptions = LayoutOptions.Center
                        )
                    ]                 
                )
                yield View.StackLayout(
                    horizontalOptions = LayoutOptions.EndAndExpand,
                    orientation = StackOrientation.Horizontal,
                    children = [
                        yield View.Button(
                            text = "remove",
                            cornerRadius = 24,
                            widthRequest = 48.0,
                            heightRequest = 48.0,
                            verticalOptions = LayoutOptions.Center,
                            command = (fun () -> decrease item)
                        )
                        yield View.Button(
                            text = "add",
                            cornerRadius = 24,
                            widthRequest = 48.0,
                            heightRequest = 48.0,
                            verticalOptions = LayoutOptions.Center,
                            command = (fun () -> increase item)
                        )
                    ]                 
                )
            ]         
        )
        
    let addFoodstuffButton dispatch =
        View.Button(text = "Add", command = (fun () -> dispatch GoToAddFoodstuffPage))
        
    let page model dispatch =
        let createItemView = itemView (ItemAmountDecreaseRequested >> dispatch) (ItemAmountIncreaseRequested >> dispatch)
        View.ContentPage(
            content = View.StackLayout(
                padding = 16.0,
                children = [
                    yield addFoodstuffButton dispatch
                    yield View.ListView(
                        items = Seq.map createItemView model.Items,
                        rowHeight = 64
                    )
                ]
            )
        )
        
    let withBackButton value (view: ViewElement) =
        view.HasBackButton(value)
        
    let addFoodstuffPage model dispatch =
        SearchFoodstuffPage.view (AddFoodstuffPage >> dispatch) model.AddFoodstuffPage |> withBackButton true
        
    let view dispatch = function
        | Loading -> View.ContentPage()
        | Loaded model -> 
            View.NavigationPage(
                popped = (fun args -> dispatch GoToRootPage),
                pages = [
                    yield page model dispatch
                    if model.ShowAddFoodstuffPage then
                        yield addFoodstuffPage model dispatch
                ]
            )
            
