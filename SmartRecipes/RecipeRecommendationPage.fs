namespace SmartRecipes

[<RequireQualifiedAccess>]
module RecipeRecommendationPage =
    open Domain
    open FSharpPlus
    open FSharpPlus.Data
    open Fabulous.Core
    open Fabulous.DynamicViews
    open Xamarin.Forms
    
    type Model = {
        Recipes: Recipe seq
        IsLoading: bool
    }
    
    type Message =
        | AddRecipe of Recipe
        | PageRefreshed of Recipe seq
        
    // Initialization
    
    let initModel = {
        Recipes = Seq.empty
        IsLoading = true
    }
    
    // Update

    let update model msg env =
        match msg with
        | AddRecipe recipe ->
            model, Cmd.none
        | PageRefreshed recipes ->
            { model with Recipes = recipes }, Cmd.none
        
    // View
    
    let recipesList dispatch recipes =
        View.ListView(
            rowHeight = 128,
            separatorVisibility = SeparatorVisibility.None,
            items = Seq.map Elements.recipeCard recipes
        )
        
    let mainContent dispatch model =
        View.StackLayout(
            padding = 16.0,
            children = [
                yield recipesList dispatch model.Recipes
            ]
        )
    
    let mainPage dispatch model =
        View.ContentPage(
            content = mainContent dispatch model
        )
    
    let view dispatch model =
        View.NavigationPage(
            title = "Suggestions",
            pages = [
                yield mainPage dispatch model
            ]
        )
