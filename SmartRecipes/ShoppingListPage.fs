namespace SmartRecipes

module ShoppingListPage =
    open Fabulous.Core
    open Fabulous.DynamicViews
    open Xamarin.Forms
    
    type FoodstuffId = FoodstuffId of string
    
    type Amount = {
        Value: float
        Unit: string
    }
    
    type Foodstuff = {
        Id: FoodstuffId
        Name: string
        Unit: string
        AmounStep: Amount
    }
    
    type Item = {
        Foodstuff: Foodstuff
        Amount: float
    }
    
    type Model = {
        Items: Item list
        IsLoading: bool
    }
    
    let initModel = {
        Items = List.empty
        IsLoading = true
    }
    
    type Message =
        | PageLoaded of Item list
        | ItemAmountIncreaseRequested of Item
        | ItemAmountIncreased of Item
        | ItemAmountDecreaseRequested of Item
        | ItemAmountDecreased of Item
        | ItemRemoved of Item
        | AddItem
        
    let update model = function
        | PageLoaded items -> ({ model with Items = items; IsLoading = false }, Cmd.none)
        | ItemAmountIncreaseRequested id -> (model, Cmd.none)
        | ItemAmountDecreased id -> (model, Cmd.none)
        | ItemAmountDecreaseRequested id -> (model, Cmd.none)
        | ItemAmountIncreased id -> (model, Cmd.none)
        | ItemRemoved id -> (model, Cmd.none)
        | AddItem -> (model, Cmd.none)

    let itemView increase decrease item =
        View.ViewCell(
            view = View.StackLayout(
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
                                text = item.Amount.ToString() + " " + item.Foodstuff.Unit,
                                verticalOptions = LayoutOptions.Center
                            )
                        ]                 
                    )
                    yield View.StackLayout(
                        horizontalOptions = LayoutOptions.EndAndExpand,
                        orientation = StackOrientation.Horizontal,
                        children = [
                            yield View.Button(
                                image = "remove",
                                cornerRadius = 24,
                                widthRequest = 48.0,
                                heightRequest = 48.0,
                                verticalOptions = LayoutOptions.Center,
                                command = (fun () -> decrease item)
                            )
                            yield View.Button(
                                image = "add",
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
        )
        
    let view model dispatch =
        let createItemView = itemView (ItemAmountDecreaseRequested >> dispatch) (ItemAmountIncreaseRequested >> dispatch)
        View.ContentPage(
            content = View.StackLayout(
                padding = 16.0,
                children = [
                    yield View.ListView(
                        items = List.map createItemView model.Items,
                        rowHeight = 64
                    )
                ]
            )
        )
