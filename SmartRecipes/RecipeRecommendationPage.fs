namespace SmartRecipes

[<RequireQualifiedAccess>]
module RecipeRecommendationPage =
    open Domain
    open FSharpPlus
    open FSharpPlus.Data
    open Fabulous.Core
    open Fabulous.DynamicViews
    
    type Model = {
        Recipes: Recipe seq
        IsLoading: bool
    }
    
    type Message =
        | AddRecipe of Recipe
        
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
        
    // View
    
    let view dispatch model =
        View.NavigationPage(
            pages = [
                yield View.ContentPage()
            ]
        )
