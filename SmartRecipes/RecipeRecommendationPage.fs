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
        | PageRefreshed of Recipe seq
        | GoToRecipeDetail of Recipe
        | HideRecipeDetail
        | RecipeDetailMessage of RecipeDetailPage.Message
        
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
        | PageRefreshed recommendations ->
            { model with Recommendations = recommendations; IsLoading = false }, Cmd.none
        | GoToRecipeDetail recipe ->
            { model with RecipeDetailPageState = Visible <| RecipeDetailPage.initModel recipe true }, Cmd.none
        | HideRecipeDetail ->
            { model with RecipeDetailPageState = Hidden }, Cmd.none
        
    // View
    
    let recipeDetailPage dispatch = function
        | Hidden -> []
        | Visible recipeDetailModel -> [ RecipeDetailPage.view (RecipeDetailMessage >> dispatch) recipeDetailModel ]
    
    let recommendationCard recipe =
        Elements.recipeCard recipe []
    
    let recommendationList dispatch isLoading recipes =
        let refresh = if isLoading then ListRefresh.Refreshing else ListRefresh.Some (fun () -> dispatch Refresh)
        Elements.cardList recipes recommendationCard (GoToRecipeDetail >> dispatch) refresh

    let mainContent dispatch isLoading recipes =
         View.Grid(
            padding = 16.0,
            rowdefs = [box "*"],
            rowSpacing = 0.0,
            children = [
                yield (recommendationList dispatch isLoading recipes).GridRow(0)
                yield View.Label(
                    text = "Go add some ingredients first !",
                    horizontalTextAlignment = TextAlignment.Center,
                    verticalOptions = LayoutOptions.Center,
                    fontSize = Elements.headingFontSize,
                    isVisible = (recipes.Length = 0)
                ).GridRow(0)
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
                yield! recipeDetailPage dispatch model.RecipeDetailPageState
            ]
        )