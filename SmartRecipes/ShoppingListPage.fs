namespace SmartRecipes

[<RequireQualifiedAccess>]
module ShoppingListPage =
    open Api
    open Domain
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
        | ShoppingListChanged of ShoppingList
        
    // Initialization
    
    let initModel = Loading
    
    let private getShoppingList (api: Api.SmartRecipesApi) accessToken = async {
        let! shoppingListResponse = api.GetShoppingList { AccessToken = accessToken }
        return shoppingListResponse.ShoppingList;
    }
    
    let private getFoodstuffs  (api: Api.SmartRecipesApi) accessToken (shoppingList: ShoppingList) = async {
        let foodstuffIds = Seq.map (fun i -> i.FoodstuffId) shoppingList.Items
        let! foodstuffResponse = api.GetFoodstuffsById { Ids = foodstuffIds; AccessToken = accessToken }
        let foodstuffs = foodstuffResponse.Foodstuffs;
        return Seq.map (fun (f: Foodstuff) -> (f.Id, f)) foodstuffs |> Map.ofSeq
    }
    
    let private toItems foodstuffs =
         Seq.map (fun i -> { Foodstuff = Map.find i.FoodstuffId foodstuffs; Amount = i.Amount })
    
    let init api accessToken = async {
        let! shoppingList = getShoppingList api accessToken;
        let! foodstuffs = getFoodstuffs api accessToken shoppingList ;
        let items = toItems foodstuffs shoppingList.Items
        return PageLoaded {
            Items = items
            AccessToken = accessToken
            AddFoodstuffPage = SearchFoodstuffPage.initModel accessToken api
            ShowAddFoodstuffPage = false
            Api = api
        }
    }
    
    // Update
    
    let tryAddFoodstuff model (foodstuff: Foodstuff) =
        model.Api.AddFoodstuffsToShoppingList { Ids = [| foodstuff.Id |]; AccessToken = model.AccessToken }
        |> Async.map (fun r -> ShoppingListChanged r.ShoppingList)
    
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
                    Loaded m, tryAddFoodstuff m f |> Cmd.ofAsyncMsg
            | ShoppingListChanged shoppingList ->
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
            
