namespace SmartRecipes

[<RequireQualifiedAccess>]
module ShoppingListRecipePage =
    open Domain
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
    
    let private getShoppingList accessToken = async {
        let! shoppingListResponse = Api.sendGetShoppingListRequest accessToken
        return shoppingListResponse.ShoppingList
    }
    
    let private getRecipes shoppingList accessToken = async {
        let recipeIds = Seq.map (fun i -> i.RecipeId) shoppingList.RecipeItems
        let! recipeResponse = Api.sendGetRecipesByIdRequest recipeIds accessToken
        return recipeResponse.Recipes
    }
    
    let private getIngredients (recipes: Recipe seq) accessToken = async {
        let foodstuffIds = Seq.collect (fun (r: Recipe) -> Seq.map (fun (i: Ingredient) -> i.FoodstuffId) r.Ingredients) recipes
        let! foodstuffResponse = Api.sendGetFoodstuffsByIdRequest foodstuffIds accessToken
        return foodstuffResponse.Foodstuffs
    }
    
    let private createItem (recipes: Map<RecipeId, Recipe>) ingredients recipeItem =
        let recipe = Map.find recipeItem.RecipeId recipes
        let foodstuffs = Seq.map (fun (i: Ingredient) -> Map.find i.FoodstuffId ingredients) recipe.Ingredients
        { PersonCount = recipeItem.PersonCount; Recipe = recipe; Ingredients = foodstuffs }
    
    let private createItems shoppingList recipes ingredients =
        let recipesById = Seq.map (fun (r: Recipe) -> (r.Id, r)) recipes |> Map.ofSeq
        let ingredientsByFoodstuffId = Seq.map (fun (i: Foodstuff) -> (i.Id, i)) ingredients |> Map.ofSeq
        Seq.map (createItem recipesById ingredientsByFoodstuffId) shoppingList.RecipeItems
    
    let init accessToken = async {
        let! shoppingList = getShoppingList accessToken
        let! recipes = getRecipes shoppingList accessToken
        let! ingredients = getIngredients recipes accessToken
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
