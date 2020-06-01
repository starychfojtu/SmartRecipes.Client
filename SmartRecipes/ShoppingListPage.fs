namespace SmartRecipes

[<RequireQualifiedAccess>]
module ShoppingListPage =
    open Api
    open AppEnvironment
    open Domain
    open FSharpPlus
    open FSharpPlus.Data
    open Fabulous.Core
    open Fabulous.DynamicViews
    open Elements
    
    type Item = {
        Foodstuff: Foodstuff
        Amount: float
    }
    
    type Model = {
        Items: Item list
        AddFoodstuffPage: SearchFoodstuffPage.Model
        ShowAddFoodstuffPage: bool
        IsLoading: bool
    }
        
    type Message =
        | Refresh
        | PageLoaded of Model
        | ItemAmountIncreaseRequested of Item
        | ItemAmountDecreaseRequested of Item
        | ItemRemoved of Item
        | GoToAddFoodstuffPage
        | GoToRootPage
        | AddFoodstuffPage of SearchFoodstuffPage.Message
        | ShoppingListChanged of Item list
        | RemoveAllItems
        
    // Initialization
    
    let initModel = {
        Items = []
        AddFoodstuffPage = SearchFoodstuffPage.initModel
        ShowAddFoodstuffPage = false
        IsLoading = true
    }
    
    let private getShoppingList = ReaderT(fun env -> 
        env.Api.GetShoppingList () |> Async.map (fun r -> r.ShoppingList))
    
    let private getFoodstuffsByIds foodstuffIds = ReaderT(fun env ->
        env.Api.GetFoodstuffsById { Ids = foodstuffIds } |> Async.map (fun r -> r.Foodstuffs))
    
    let private getFoodstuffs (shoppingList: ShoppingList) = monad {
        let foodstuffIds = Seq.map (fun i -> i.FoodstuffId) shoppingList.Items |> Seq.toList
        let! foodstuffs = getFoodstuffsByIds foodstuffIds
        return Seq.map (fun (f: Foodstuff) -> (f.Id, f)) foodstuffs |> Map.ofSeq
    }
    
    let private toItems foodstuffs =
         Seq.map (fun i -> { Foodstuff = Map.find i.FoodstuffId foodstuffs; Amount = i.Amount }) >> Seq.toList
         
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
        env.Api.AddFoodstuffsToShoppingList { ItemIds = [ id ] })
    
    let tryAddFoodstuff (foodstuff: Foodstuff) = monad {
        let! response = addFoodstuffToShoppingList foodstuff.Id
        let! command = 
            match response with
            | Ok r -> shoppingListToItems r.ShoppingList |> ReaderT.map (ShoppingListChanged >> Some)
            | Error _ -> ReaderT(fun _ -> Async.id None) 
        return command
    }
    
    let setFoodstuffAmount id value = ReaderT(fun env ->
        env.Api.SetFoodstuffAmountInShoppingList { FoodstuffId = id; Amount = value })
    
    let amountStepAction (item: Item) f = monad {
        let newValue = f item.Amount item.Foodstuff.AmountStep
        let! response = setFoodstuffAmount item.Foodstuff.Id newValue
        let! command = 
            match response with
            | Ok r -> shoppingListToItems r.ShoppingList |> ReaderT.map (ShoppingListChanged >> Some)
            | Error _ -> ReaderT(fun _ -> Async.id None) 
        return command
    }
    
    let removeFoodstuffs ids = ReaderT(fun env ->
        env.Api.RemoveFoodstuffs { Ids = ids; })
    
    let removeAllItems items = monad {
        let! response = Seq.map (fun i -> i.Foodstuff.Id) items |> Seq.toList |> removeFoodstuffs
        let! command = 
            match response with
            | Ok r -> shoppingListToItems r.ShoppingList |> ReaderT.map (ShoppingListChanged >> Some)
            | Error _ -> ReaderT(fun _ -> Async.id None) 
        return command
    }

    let tryRemoveFoodstuff (foodstuff: Foodstuff) = monad {
        let! response = removeFoodstuffs [foodstuff.Id]
        let! command = 
            match response with
            | Ok r -> shoppingListToItems r.ShoppingList |> ReaderT.map (ShoppingListChanged >> Some)
            | Error _ -> ReaderT(fun _ -> Async.id None) 
        return command
    }

    let update model msg env =
        match msg with
        | Refresh ->
            { model with IsLoading = true; }, Cmd.ofReader env init
        | PageLoaded loadedModel ->
            loadedModel, Cmd.none
        | ItemAmountIncreaseRequested item ->
            model, amountStepAction item (+) |> Cmd.ofReaderOption env
        | ItemAmountDecreaseRequested item ->
            model, amountStepAction item (-) |> Cmd.ofReaderOption env
        | ItemRemoved item ->
            model, tryRemoveFoodstuff item.Foodstuff |> Cmd.ofReaderOption env
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
                model, tryAddFoodstuff f |> Cmd.ofReaderOption env
        | ShoppingListChanged items ->
            { model with Items = items }, Cmd.none
        | RemoveAllItems ->
            model, removeAllItems model.Items |> Cmd.ofReaderOption env
        
    // View

    let itemView increase decrease remove item =
        Elements.FoodstuffCard(
            actions = [
                if item.Amount >= item.Foodstuff.AmountStep 
                    then
                        yield Elements.RoundedButton(
                            text = "-",
                            command = (fun () -> decrease item)
                        )
                    else
                        yield Elements.RoundedButton(
                            text = "✕",
                            command = (fun () -> remove item)
                        )
                yield Elements.RoundedButton(
                    text = "+",
                    command = (fun () -> increase item)
                )
            ],
            foodstuff = item.Foodstuff,
            amount = item.Amount
        )
        
    let page model dispatch =
        View.ContentPage(
            content = Elements.RefreshListPageContent(
                isLoading = model.IsLoading,
                items = List.toArray model.Items,
                itemView = (itemView (ItemAmountIncreaseRequested >> dispatch) (ItemAmountDecreaseRequested >> dispatch) (ItemRemoved >> dispatch)),
                onTapped = (fun _ -> ()),
                refresh = (fun () -> dispatch Refresh),
                emptyText = "No items :( Let's add some !",
                rowHeight = 64
            )
        )
        
    let addFoodstuffPage model dispatch =
        Seq.map (fun i -> i.Foodstuff) model.Items
        |> SearchFoodstuffPage.view (AddFoodstuffPage >> dispatch) model.AddFoodstuffPage
        |> ViewElement.withBackButton true
        
    let view dispatch model =
        let addFoodstuffToolbarItem = 
            View.ToolbarItem(    
                text = "Add",
                command = fun () -> dispatch GoToAddFoodstuffPage
            )
        
        let clearListToolbarItem = 
            View.ToolbarItem(    
                text = "Clear",
                command = fun () -> dispatch RemoveAllItems
            )
        
        View.NavigationPage(
            title = "Shopping list",
            popped = (fun args -> dispatch GoToRootPage),
            toolbarItems = [ 
                if not model.ShowAddFoodstuffPage then
                    yield addFoodstuffToolbarItem 
                    yield clearListToolbarItem
            ],
            pages = [
                yield page model dispatch
                if model.ShowAddFoodstuffPage then
                    yield addFoodstuffPage model dispatch
            ]
        )
            
