namespace SmartRecipes
open Elements
open Library

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
        RecipeDetailPageState: PageState<RecipeDetailPage.Model>
    }
    
    type Message =
        | Refresh
        | RecipeAdded of Recipe
        | PageRefreshed of Recipe seq
        | GoToRecipeDetail of Recipe
        | HideRecipeDetail
        
    // Initialization
    
    let initModel = {
        Recommendations = Seq.empty
        IsLoading = true
        RecipeDetailPageState = Hidden
    }
    
    let getRecommendedRecipes = ReaderT(fun env ->
        env.Api.GetRecommendedRecipes ())
    
    let init =
        getRecommendedRecipes
        |> ReaderT.map (fun r -> PageRefreshed r.Recipes)
    
    // Update
    
    let update model msg env =
        match msg with
        | Refresh ->
            { model with IsLoading = true }, init |> Cmd.ofReader env
        | RecipeAdded recipe ->
            failwith "not implemented"
        | PageRefreshed recommendations ->
            { model with Recommendations = recommendations; IsLoading = false }, Cmd.none
        | GoToRecipeDetail recipe ->
            { model with RecipeDetailPageState = Visible <| RecipeDetailPage.initModel recipe true }, Cmd.none
        | HideRecipeDetail ->
            { model with RecipeDetailPageState = Hidden }, Cmd.none
        
    // View
    
    let recommendationCard recipe =
        Elements.recipeCard recipe []
    
    let recommendationList dispatch isLoading recipes =
        let refresh = if isLoading then ListRefresh.Refreshing else ListRefresh.Some (fun () -> dispatch Refresh)
        Elements.cardList recipes recommendationCard (GoToRecipeDetail >> dispatch) refresh
        
    let mainContent dispatch isLoading recipes =
        View.StackLayout(
            padding = 16.0,
            children = [
                yield recommendationList dispatch isLoading recipes
            ]
        )
    
    let mainPage dispatch isLoading recipes =
        View.ContentPage(
            content = mainContent dispatch isLoading recipes
        )
    
    let view dispatch model =
        View.NavigationPage(
            title = "Suggestions",
            popped = (fun _ -> dispatch HideRecipeDetail),
            pages = [
                yield dependsOn (model.Recommendations, model.IsLoading) (fun model (rs, isLoading) -> mainPage dispatch isLoading (Seq.toArray rs))
            ]
        )