namespace SmartRecipes

[<RequireQualifiedAccess>]
module ShoppingListPage =
    open Api
    open AppEnvironment
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
    
    type Model = {
        Items: Item seq
        AddFoodstuffPage: SearchFoodstuffPage.Model
        ShowAddFoodstuffPage: bool
        IsLoading: bool
    }
        
    type Message =
        | PageLoaded of Model
        | ItemAmountIncreaseRequested of Item
        | ItemAmountDecreaseRequested of Item
        | ItemRemoved of Item
        | GoToAddFoodstuffPage
        | GoToRootPage
        | AddFoodstuffPage of SearchFoodstuffPage.Message
        | ShoppingListChanged of Item seq
        
    // Initialization
    
    let initModel = {
        Items = Seq.empty
        AddFoodstuffPage = SearchFoodstuffPage.initModel
        ShowAddFoodstuffPage = false
        IsLoading = true
    }
    
    let private getShoppingList = ReaderT(fun env -> 
        env.Api.GetShoppingList () |> Async.map (fun r -> r.ShoppingList))
    
    let private getFoodstuffsByIds foodstuffIds = ReaderT(fun env ->
        env.Api.GetFoodstuffsById { Ids = foodstuffIds } |> Async.map (fun r -> r.Foodstuffs))
    
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
         
    let private getItems = monad {
        let! shoppingList = getShoppingList
        return! shoppingListToItems shoppingList
    }
    
    let private createModel items =
        PageLoaded {
            Items = items
            AddFoodstuffPage = SearchFoodstuffPage.initModel
            ShowAddFoodstuffPage = false
            IsLoading = false
        }
    
    let init = monad {
        let! items = getItems
        return createModel items
    }
    
    // Update
    
    let addFoodstuffToShoppingList id = ReaderT(fun env ->
        env.Api.AddFoodstuffsToShoppingList { Ids = [| id |] })
    
    let tryAddFoodstuff (foodstuff: Foodstuff) = monad {
        let! response = addFoodstuffToShoppingList foodstuff.Id
        let! items = shoppingListToItems response.ShoppingList
        return ShoppingListChanged items
    }
    
    let setFoodstuffAmount id value = ReaderT(fun env ->
        env.Api.SetFoodstuffAmountInShoppingList { Id = id; Value = value })
    
    let amountStepAction (item: Item) f = monad {
        let newValue = f item.Amount item.Foodstuff.AmountStep.Value
        let! response = setFoodstuffAmount item.Foodstuff.Id newValue
        let! items = shoppingListToItems response.ShoppingList
        return ShoppingListChanged items
    }

    let update model msg env =
            match msg with
            | PageLoaded loadedModel ->
                loadedModel, Cmd.none
            | ItemAmountIncreaseRequested item ->
                model, amountStepAction item (+) |> Cmd.ofReader env
            | ItemAmountDecreaseRequested item ->
                model, amountStepAction item (-) |> Cmd.ofReader env
            | ItemRemoved id ->
                model, Cmd.none
            | GoToAddFoodstuffPage ->
                { model with ShowAddFoodstuffPage = true }, Cmd.none
            | GoToRootPage ->
                { model with ShowAddFoodstuffPage = false }, Cmd.none
            | AddFoodstuffPage addFoodstuffPageMessage ->
                let updateResult = SearchFoodstuffPage.update model.AddFoodstuffPage addFoodstuffPageMessage env
                match updateResult with
                | SearchFoodstuffPage.UpdateResult.ModelUpdated (newModel, cmd) ->
                    { model with AddFoodstuffPage = newModel }, Cmd.map AddFoodstuffPage cmd
                | SearchFoodstuffPage.UpdateResult.FoodstuffSelected f ->
                    model, tryAddFoodstuff f |> Cmd.ofReader env
            | ShoppingListChanged items ->
                { model with Items = items }, Cmd.none
        
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
        let createItemView = itemView (ItemAmountIncreaseRequested >> dispatch) (ItemAmountDecreaseRequested >> dispatch)
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
        
    let addFoodstuffPage model dispatch =
        let foodstuffs = Seq.map (fun i -> i.Foodstuff) model.Items
        SearchFoodstuffPage.view (AddFoodstuffPage >> dispatch) model.AddFoodstuffPage foodstuffs
        |> ViewElement.withBackButton true
        
    let view dispatch model = 
        View.NavigationPage(
            popped = (fun args -> dispatch GoToRootPage),
            pages = [
                yield page model dispatch
                if model.ShowAddFoodstuffPage then
                    yield addFoodstuffPage model dispatch
            ]
        )
            
