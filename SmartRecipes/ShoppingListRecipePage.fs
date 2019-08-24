namespace SmartRecipes
open Elements
open Library

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
    
    type Model = {
        Items: Item list
        IsLoading: bool
        SearchPageState: PageState<SearchRecipePage.Model>
        RecipeDetailPageState: PageState<RecipeDetailPage.Model>
    }
        
    type Message =
        | Refresh
        | ItemsChanged of Item seq
        | RecipeAdded of Recipe
        | RecipeRemoved of Recipe
        | SearchMessage of SearchRecipePage.Message
        | RecipeDetailMessage of RecipeDetailPage.Message
        | GoToSearch
        | HideSearch
        | GoToRecipeDetail of Recipe
        | HideRecipeDetail
        
    // Initialization
    
    let initModel = {
        Items = []
        IsLoading = true
        SearchPageState = Hidden
        RecipeDetailPageState = Hidden
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
        | Refresh ->
            { model with IsLoading = true }, Cmd.ofReader env init
        | ItemsChanged items ->
            { model with Model.Items = Seq.toList items; IsLoading = false }, Cmd.none
        | RecipeAdded recipe ->
            model, addRecipeToShoppingList recipe |> Cmd.ofReader env    
        | RecipeRemoved recipe ->
            model, removeRecipeFromShoppingList recipe |> Cmd.ofReader env
        | GoToSearch ->
            { model with SearchPageState = Visible SearchRecipePage.initModel }, Cmd.none
        | HideSearch ->
            { model with SearchPageState = Hidden }, Cmd.none
        | GoToRecipeDetail recipe ->
            let isRecipeAdded = Seq.exists (fun i -> i.Recipe = recipe) model.Items
            let initModel = RecipeDetailPage.initModel recipe (not isRecipeAdded)
            { model with RecipeDetailPageState = Visible <| initModel }, Cmd.none
        | HideRecipeDetail ->
            { model with RecipeDetailPageState = Hidden }, Cmd.none
        | SearchMessage searchMsg ->
            match model.SearchPageState with
            | Hidden ->
                model, Cmd.none
            | Visible searchModel ->  
                let updateResult = SearchRecipePage.update searchModel searchMsg env
                match updateResult with
                | SearchRecipePage.UpdateResult.ModelUpdated (newSearchModel, searchCmd) -> 
                    { model with SearchPageState = Visible newSearchModel }, Cmd.map SearchMessage searchCmd
                | SearchRecipePage.UpdateResult.RecipeSelected recipe ->
                    model, Cmd.ofMsg <| GoToRecipeDetail recipe
        | RecipeDetailMessage recipeDetailMessage ->
            match model.RecipeDetailPageState with
            | Hidden ->
                model, Cmd.none
            | Visible recipeDetailModel ->
                let updateResult = RecipeDetailPage.update recipeDetailModel recipeDetailMessage
                match updateResult with
                | RecipeDetailPage.UpdateResult.RecipeAdded ->
                    let newState = Visible <| { recipeDetailModel with CanBeAdded = false }
                    { model with RecipeDetailPageState = newState }, Cmd.ofMsg <| RecipeAdded recipeDetailModel.Recipe
        
    // View
    
    let searchPage dispatch model =
        let ignoredRecipes = Seq.map (fun i -> i.Recipe) model.Items
        match model.SearchPageState with
        | Hidden ->
            Option.None
        | Visible searchModel ->
            Option.Some <| SearchRecipePage.view (SearchMessage >> dispatch) searchModel ignoredRecipes
            
    let recipeDetailPage dispatch model =
        match model.RecipeDetailPageState with
        | Hidden ->
            Option.None
        | Visible recipeDetailModel ->
            Option.Some <| RecipeDetailPage.view (RecipeDetailMessage >> dispatch) recipeDetailModel
    
    let pages dispatch model =
        List.choose id [ searchPage dispatch model; recipeDetailPage dispatch model ]
            
    let searchRecipeToolbarItem dispatch = 
        View.ToolbarItem(
            text = "Search",
            command = fun () -> dispatch GoToSearch
        )
        
    let recipeItemCard dispatch item =
        Elements.RecipeCard(
            actionItems = [ Elements.RoundedButton(
                text = "-",
                command = (fun () -> RecipeRemoved item.Recipe |> dispatch)
            ) ],
            recipe = item.Recipe
        )
        
    let mainPage dispatch model  =
        View.ContentPage(
            content = Elements.RefreshListPageContent(
                isLoading = model.IsLoading,
                items = List.toArray model.Items,
                itemView = recipeItemCard dispatch,
                onTapped = (fun i -> GoToRecipeDetail i.Recipe |> dispatch),
                refresh = (fun () -> dispatch Refresh),
                emptyText = "No recipes, checkout your suggestions :)",
                rowHeight = 128
            )
        )

    let view dispatch model =
        View.NavigationPage(
            title = "Recipes",
            toolbarItems = [
                searchRecipeToolbarItem dispatch
            ],
            popped = (fun _ ->
                match model.RecipeDetailPageState with
                | Visible _ -> dispatch HideRecipeDetail
                | Hidden ->
                    match model.SearchPageState with
                    | Visible _ -> dispatch HideSearch
                    | Hidden -> ()
            ),
            pages = [
                yield mainPage dispatch model
                yield! pages dispatch model
            ]
        )
        
