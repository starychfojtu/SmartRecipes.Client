namespace SmartRecipes

[<RequireQualifiedAccess>]
module ShoppingListRecipePage =
    open Domain
    open FSharpPlus.Data
    open FSharpx.Control
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
    
    type LoadedModel = {
        Items: Item seq
    }
    
    type Model =
        | Loading
        | Loaded of LoadedModel
        
    type Message =
        | PageLoaded of LoadedModel
        
    // Initialization
    
    let initModel = Loading
    
    let private getShoppingList = ReaderT(fun env ->
        env.Api.GetShoppingList () |> Async.map (fun r -> r.ShoppingList))
    
    let private getRecipesById ids = ReaderT(fun env ->
        env.Api.GetRecipesById { Ids = ids } |> Async.map (fun r -> r.Recipes))
    
    let private getFoodstuffById ids = ReaderT(fun env ->
        env.Api.GetFoodstuffsById { Ids = ids } |> Async.map (fun r -> r.Foodstuffs))
    
    let private getRecipes shoppingList = monad {
        let recipeIds = Seq.map (fun i -> i.RecipeId) shoppingList.RecipeItems
        return! getRecipesById recipeIds
    }
    
    let private getIngredients (recipes: Recipe seq) = monad {
        let foodstuffIds = Seq.collect (fun (r: Recipe) -> Seq.map (fun (i: Ingredient) -> i.FoodstuffId) r.Ingredients) recipes
        return! getFoodstuffById foodstuffIds
    }
    
    let private createItem (recipes: Map<RecipeId, Recipe>) ingredients recipeItem =
        let recipe = Map.find recipeItem.RecipeId recipes
        let foodstuffs = Seq.map (fun (i: Ingredient) -> Map.find i.FoodstuffId ingredients) recipe.Ingredients
        { PersonCount = recipeItem.PersonCount; Recipe = recipe; Ingredients = foodstuffs }
    
    let private createItems shoppingList recipes ingredients =
        let recipesById = Seq.map (fun (r: Recipe) -> (r.Id, r)) recipes |> Map.ofSeq
        let ingredientsByFoodstuffId = Seq.map (fun (i: Foodstuff) -> (i.Id, i)) ingredients |> Map.ofSeq
        Seq.map (createItem recipesById ingredientsByFoodstuffId) shoppingList.RecipeItems
    
    let init = monad {
        let! shoppingList = getShoppingList
        let! recipes = getRecipes shoppingList
        let! ingredients = getIngredients recipes
        let items = createItems shoppingList recipes ingredients
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
            | _ -> failwith "Unhandled message"
        
    // View

    let itemView item =
        View.Frame(
            margin = Thickness(16.0, 8.0),
            content = View.StackLayout(
                children = [
                    yield View.StackLayout(
                        orientation = StackOrientation.Horizontal,
                        heightRequest = 64.0,
                        padding = Thickness(8.0, 0.0),
                        children = [
                            yield View.Label(
                                text = item.Recipe.Name,
                                horizontalOptions = LayoutOptions.Start,
                                verticalOptions = LayoutOptions.Center
                            )
                        ]
                    )
                    yield View.StackLayout(
                        orientation = StackOrientation.Horizontal,
                        heightRequest = 40.0,
                        padding = Thickness(8.0, 4.0),
                        children = [
                            yield View.Label(
                                text = item.PersonCount.ToString (),
                                margin = Thickness(8.0, 0.0, 0.0, 0.0),
                                horizontalOptions = LayoutOptions.Start,
                                textColor = Color.Black,
                                fontSize = "Medium",
                                verticalOptions = LayoutOptions.Center
                            )
                        ]
                    )
                ]
            )
        )
        
    let view dispatch = function
        | Loading -> View.ContentPage()
        | Loaded m ->
            View.ContentPage(
                content = View.StackLayout(
                    padding = 16.0,
                    children = [
                        yield View.ListView(
                            items = Seq.map itemView m.Items,
                            rowHeight = 128,
                            separatorVisibility = SeparatorVisibility.None
                        )
                    ]
                )
            )
