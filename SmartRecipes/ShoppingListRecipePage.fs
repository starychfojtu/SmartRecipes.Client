namespace SmartRecipes

[<RequireQualifiedAccess>]
module ShoppingListRecipePage =
    open Domain
    open FSharpPlus.Data
    open AppEnvironment
    open FSharpPlus
    open Fabulous.Core
    open Fabulous.DynamicViews
    open Xamarin.Forms
    
    type Item = {
        Recipe: Recipe
        PersonCount: int
        Ingredients: Foodstuff seq
    }
    
    type RecipeDetailPageState =
        | Hidden
        | Visible of RecipeDetailPage.Model
    
    type SearchPageState =
        | Hidden
        | Visible of SearchRecipePage.Model
    
    type Model = {
        Items: Item seq
        IsLoading: bool
        SearchPageState: SearchPageState
        RecipeDetailPageState: RecipeDetailPageState
    }
        
    type Message =
        | ItemsChanged of Item seq
        | RecipeAdded of Recipe
        | RecipeRemoved of Recipe
        | SearchMessage of SearchRecipePage.Message
        | RecipeDetailMessage of RecipeDetailPage.Message
        | GoToSearch
        | GoToRecipeDetail of Recipe
        
    // Initialization
    
    let initModel = {
        Items = Seq.empty
        IsLoading = true
        SearchPageState = SearchPageState.Hidden
        RecipeDetailPageState = RecipeDetailPageState.Hidden
    }
    
    let private getShoppingList = ReaderT(fun env ->
        env.Api.GetShoppingList () |> Async.map (fun r -> r.ShoppingList))
    
    let private getRecipesById ids = ReaderT(fun env ->
        env.Api.GetRecipesById { Ids = ids } |> Async.map (fun r -> r.Recipes))
    
    let private getFoodstuffById ids = ReaderT(fun env ->
        env.Api.GetFoodstuffsById { Ids = ids } |> Async.map (fun r -> r.Foodstuffs))
    
    let private getRecipes shoppingList = monad {
        let recipeIds = Seq.map (fun i -> i.RecipeId) shoppingList.Recipes |> Seq.toList
        return! getRecipesById recipeIds
    }
    
    let private getIngredients (recipes: Recipe seq) = monad {
        let foodstuffIds = Seq.collect (fun (r: Recipe) -> Seq.map (fun (i: Ingredient) -> i.FoodstuffId) r.Ingredients) recipes
        return! getFoodstuffById <| Seq.toList foodstuffIds
    }
    
    let private createItem (recipes: Map<RecipeId, Recipe>) ingredients recipeItem =
        let recipe = Map.find recipeItem.RecipeId recipes
        let foodstuffs = Seq.map (fun (i: Ingredient) -> Map.find i.FoodstuffId ingredients) recipe.Ingredients
        { PersonCount = recipeItem.PersonCount; Recipe = recipe; Ingredients = foodstuffs }
    
    let private createItems shoppingList recipes ingredients =
        let recipesById = Seq.map (fun (r: Recipe) -> (r.Id, r)) recipes |> Map.ofSeq
        let ingredientsByFoodstuffId = Seq.map (fun (i: Foodstuff) -> (i.Id, i)) ingredients |> Map.ofSeq
        Seq.map (createItem recipesById ingredientsByFoodstuffId) shoppingList.Recipes
        
    let private shoppingListToItems shoppingList =  monad {
        let! recipes = getRecipes shoppingList
        let! ingredients = getIngredients recipes
        return createItems shoppingList recipes ingredients
    }
    
    let init = monad {
        let! shoppingList = getShoppingList
        let! items = shoppingListToItems shoppingList
        return ItemsChanged items
    }
    
    // Update
    
    let addRecipesToShoppingList ids = ReaderT(fun env ->
        env.Api.AddRecipesToShoppingList { ItemIds = ids })
    
    let addRecipeToShoppingList (recipe: Recipe) = monad {
        let! response = addRecipesToShoppingList [ recipe.Id ]
        let! items = shoppingListToItems response.ShoppingList
        return ItemsChanged items
    }
    
    let removeRecipesFromShoppingList ids = ReaderT(fun env ->
        env.Api.RemoveRecipesFromShoppingList { Ids = ids })
    
    let removeRecipeFromShoppingList (recipe: Recipe) = monad {
        let! response = removeRecipesFromShoppingList [ recipe.Id ]
        let! items = shoppingListToItems response.ShoppingList
        return ItemsChanged items
    }
    
    let update model msg env =
        match msg with
        | ItemsChanged items ->
            { model with Model.Items = items }, Cmd.none
        | RecipeAdded recipe ->
            model, addRecipeToShoppingList recipe |> Cmd.ofReader env    
        | RecipeRemoved recipe ->
            model, removeRecipeFromShoppingList recipe |> Cmd.ofReader env
        | GoToSearch ->
            { model with SearchPageState = SearchPageState.Visible SearchRecipePage.initModel }, Cmd.none
        | GoToRecipeDetail recipe ->
            let isRecipeAdded = Seq.exists (fun i -> i.Recipe = recipe) model.Items
            let initModel = RecipeDetailPage.initModel recipe (not isRecipeAdded)
            { model with RecipeDetailPageState = RecipeDetailPageState.Visible <| initModel }, Cmd.none
        | SearchMessage searchMsg ->
            match model.SearchPageState with
            | SearchPageState.Hidden ->
                model, Cmd.none
            | SearchPageState.Visible searchModel ->  
                let updateResult = SearchRecipePage.update searchModel searchMsg env
                match updateResult with
                | SearchRecipePage.UpdateResult.ModelUpdated (newSearchModel, searchCmd) -> 
                    { model with SearchPageState = SearchPageState.Visible newSearchModel }, Cmd.map SearchMessage searchCmd
                | SearchRecipePage.UpdateResult.RecipeSelected recipe ->
                    model, Cmd.ofMsg <| GoToRecipeDetail recipe
        | RecipeDetailMessage recipeDetailMessage ->
            match model.RecipeDetailPageState with
            | RecipeDetailPageState.Hidden ->
                model, Cmd.none
            | RecipeDetailPageState.Visible recipeDetailModel ->
                let updateResult = RecipeDetailPage.update recipeDetailModel recipeDetailMessage
                match updateResult with
                | RecipeDetailPage.UpdateResult.RecipeAdded ->
                    let newState = RecipeDetailPageState.Visible <| { recipeDetailModel with CanBeAdded = false }
                    { model with RecipeDetailPageState = newState }, Cmd.ofMsg <| RecipeAdded recipeDetailModel.Recipe
        
    // View
    
    let recipeItemCard dispatch item =
        Elements.recipeCard item.Recipe [ Elements.actionButton "Remove" (fun () -> RecipeRemoved item.Recipe |> dispatch) ]
        
    let searchPage dispatch model =
        let ignoredRecipes = Seq.map (fun i -> i.Recipe) model.Items
        match model.SearchPageState with
        | SearchPageState.Hidden ->
            None
        | SearchPageState.Visible searchModel ->
            Some <| SearchRecipePage.view (SearchMessage >> dispatch) searchModel ignoredRecipes
            
    let recipeDetailPage dispatch model =
        match model.RecipeDetailPageState with
        | RecipeDetailPageState.Hidden ->
            None
        | RecipeDetailPageState.Visible recipeDetailModel ->
            Some <| RecipeDetailPage.view (RecipeDetailMessage >> dispatch) recipeDetailModel
    
    let pages dispatch model =
        List.choose id [ searchPage dispatch model; recipeDetailPage dispatch model ]
            
    let searchRecipeToolbarItem dispatch = 
        View.ToolbarItem(
            text = "Search",
            command = fun () -> dispatch GoToSearch
        )

    let view dispatch model =
        let recipesArray = Seq.map (fun i -> i.Recipe) model.Items |> Seq.toArray
        View.NavigationPage(
            title = "Recipes",
            toolbarItems = [
                searchRecipeToolbarItem dispatch
            ],
            pages = [
                yield View.ContentPage(
                    content = View.StackLayout(
                        padding = 16.0,
                        children = [
                            yield View.ListView(
                                items = Seq.map (recipeItemCard dispatch) model.Items,
                                rowHeight = 128,
                                separatorVisibility = SeparatorVisibility.None,
                                selectionMode = ListViewSelectionMode.None,
                                itemTapped = (fun i -> recipesArray.[i] |> GoToRecipeDetail |> dispatch)
                            )
                        ]
                    )
                )
                yield! pages dispatch model
            ]
        )
        
