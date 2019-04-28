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
        Recommendations: Recommendation seq
        IsLoading: bool
    }
    
    type Message =
        | RecipeAdded of Recipe
        | PageRefreshed of Recommendation seq
        
    // Initialization
    
    let initModel = {
        Recommendations = Seq.empty
        IsLoading = true
    }
    
    let getRecommendedRecipes = ReaderT(fun env ->
        env.Api.GetRecommendedRecipes ())
    
    let init =
        getRecommendedRecipes
        |> ReaderT.map (fun r -> PageRefreshed r.Recommendations)
    
    // Update
    
    type UpdateResult =
        | ModelUpdated of Model * Message Cmd
        | RecipeSelected of Recipe
    
    let update model msg env =
        match msg with
        | RecipeAdded recipe ->
            RecipeSelected recipe
        | PageRefreshed recommendations ->
            ModelUpdated ({ model with Recommendations = recommendations }, Cmd.none)
        
    // View
    
    let recommendationCard dispatch recommendation =
        let recipe = recommendation.Recipe
        Elements.recipeCard recipe (fun () -> RecipeAdded recipe |> dispatch)
    
    let recommendationList dispatch recommendations =
        let items =
            recommendations
            |> Seq.sortBy (fun r -> r.Priority)
            |> Seq.map (recommendationCard dispatch)
            
        View.ListView(
            rowHeight = 128,
            separatorVisibility = SeparatorVisibility.None,
            items = items
        )
        
    let mainContent dispatch model =
        View.StackLayout(
            padding = 16.0,
            children = [
                yield recommendationList dispatch model.Recommendations
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
