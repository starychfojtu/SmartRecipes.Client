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
    open Xamarin.Forms
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
        let! items = shoppingListToItems response.ShoppingList
        return ShoppingListChanged items
    }
    
    let setFoodstuffAmount id value = ReaderT(fun env ->
        env.Api.SetFoodstuffAmountInShoppingList { FoodstuffId = id; Amount = value })
    
    let amountStepAction (item: Item) f = monad {
        let newValue = f item.Amount item.Foodstuff.AmountStep
        let! response = setFoodstuffAmount item.Foodstuff.Id newValue
        let! items = shoppingListToItems response.ShoppingList
        return ShoppingListChanged items
    }
    
    let removeFoodstuffs ids = ReaderT(fun env ->
        env.Api.RemoveFoodstuffs { Ids = ids; })
    
    let removeAllItems items = monad {
        let! response = Seq.map (fun i -> i.Foodstuff.Id) items |> Seq.toList |> removeFoodstuffs 
        let! items = shoppingListToItems response.ShoppingList
        return ShoppingListChanged items
    }

    let update model msg env =
        match msg with
        | Refresh ->
            { model with IsLoading = true; }, Cmd.ofReader env init
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
        | RemoveAllItems ->
            model, removeAllItems model.Items |> Cmd.ofReader env
        
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
                            text = item.Amount.ToString() + " " + item.Foodstuff.BaseAmount.Unit.ToString(),
                            verticalOptions = LayoutOptions.Center
                        )
                    ]                 
                )
                yield View.StackLayout(
                    horizontalOptions = LayoutOptions.EndAndExpand,
                    orientation = StackOrientation.Horizontal,
                    children = [
                        if item.Amount >= item.Foodstuff.AmountStep then
                            yield Elements.actionButton "-" (fun () -> decrease item)
                        yield Elements.actionButton "+" (fun () -> increase item)
                    ]                 
                )
            ]         
        )
        
    let page model dispatch =
        View.ContentPage(
            content = View.RefreshListPageContent(
                isLoading = model.IsLoading,
                items = List.toArray model.Items,
                itemView = (itemView (ItemAmountIncreaseRequested >> dispatch) (ItemAmountDecreaseRequested >> dispatch)),
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
            
