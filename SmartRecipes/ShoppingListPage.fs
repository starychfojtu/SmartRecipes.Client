namespace SmartRecipes

[<RequireQualifiedAccess>]
module ShoppingListPage =
    open Domain
    open Fabulous.Core
    open Fabulous.DynamicViews
    open Xamarin.Forms
    
    type Item = {
        Foodstuff: Foodstuff
        Amount: float
    }
    
    type LoadedModel = {
        Items: Item seq
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
        | AddItem
        
    // Initialization
    
    let initModel = Loading
    
    let private getShoppingList accessToken = async {
        let! shoppingListResponse = Api.sendGetShoppingListRequest accessToken
        return shoppingListResponse.ShoppingList;
    }
    
    let private getFoodstuffs (shoppingList: ShoppingList) accessToken = async {
        let foodstuffIds = Seq.map (fun i -> i.FoodstuffId) shoppingList.Items
        let! foodstuffResponse = Api.sendGetFoodstuffsByIdRequest foodstuffIds accessToken
        let foodstuffs = foodstuffResponse.Foodstuffs;
        return Seq.map (fun (f: Foodstuff) -> (f.Id, f)) foodstuffs |> Map.ofSeq
    }
    
    let private toItems foodstuffs =
         Seq.map (fun i -> { Foodstuff = Map.find i.FoodstuffId foodstuffs; Amount = i.Amount })
    
    let init accessToken = async {
        let! shoppingList = getShoppingList accessToken;
        let! foodstuffs = getFoodstuffs shoppingList accessToken;
        let items = toItems foodstuffs shoppingList.Items
        return PageLoaded { Items = items }
    }
    
    // Update
    
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
                (Loaded m, Cmd.none)
            | ItemAmountDecreased id ->
                (Loaded m, Cmd.none)
            | ItemAmountDecreaseRequested id ->
                (Loaded m, Cmd.none)
            | ItemAmountIncreased id ->
                (Loaded m, Cmd.none)
            | ItemRemoved id ->
                (Loaded m, Cmd.none)
            | AddItem ->
                (Loaded m, Cmd.none)
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
        
    let view dispatch = function
        | Loading -> View.ContentPage()
        | Loaded m -> 
            let createItemView = itemView (ItemAmountDecreaseRequested >> dispatch) (ItemAmountIncreaseRequested >> dispatch)
            View.ContentPage(
                content = View.StackLayout(
                    padding = 16.0,
                    children = [
                        yield View.ListView(
                            items = Seq.map createItemView m.Items,
                            rowHeight = 64
                        )
                    ]
                )
            )
