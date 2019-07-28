namespace SmartRecipes

[<RequireQualifiedAccess>]
module RecipeRecommendationPage =
    open AppEnvironment
    open Domain
    open FSharpPlus
    open FSharpPlus.Data
    open Fabulous.Core
    open Fabulous.DynamicViews
    open Xamarin.Forms
    
    type Model = {
        Recommendations: Recipe seq
        IsLoading: bool
    }
    
    type Message =
        | Refresh
        | RecipeAdded of Recipe
        | PageRefreshed of Recipe seq
        
    // Initialization
    
    let initModel = {
        Recommendations = Seq.empty
        IsLoading = true
    }
    
    let getRecommendedRecipes = ReaderT(fun env ->
        env.Api.GetRecommendedRecipes ())
    
    let init =
        getRecommendedRecipes
        |> ReaderT.map (fun r -> PageRefreshed r.Recipes)
    
    // Update
    
    type UpdateResult =
        | ModelUpdated of Model * Message Cmd
        | RecipeSelected of Recipe
    
    let update model msg env =
        match msg with
        | Refresh ->
            ModelUpdated (model, init |> Cmd.ofReader env)
        | RecipeAdded recipe ->
            RecipeSelected recipe
        | PageRefreshed recommendations ->
            ModelUpdated ({ model with Recommendations = recommendations }, Cmd.none)
        
    // View
    
    let recommendationCard dispatch recipe =
        Elements.recipeCard recipe [ Elements.actionButton "Add" (fun () -> RecipeAdded recipe |> dispatch) ]
    
    let recommendationList dispatch recipes =
        View.ListView(
            rowHeight = 128,
            separatorVisibility = SeparatorVisibility.None,
            items = Seq.map (recommendationCard dispatch) recipes
        )
        
    let mainContent dispatch recommendations =
        View.StackLayout(
            padding = 16.0,
            children = [
                yield recommendationList dispatch recommendations
            ]
        )
    
    let mainPage dispatch recipes =
        View.ContentPage(
            content = mainContent dispatch recipes
        )
    
    let view dispatch model =
        View.NavigationPage(
            title = "Suggestions",
            pages = [
                yield dependsOn model.Recommendations (fun model -> mainPage dispatch)
            ]
        )
