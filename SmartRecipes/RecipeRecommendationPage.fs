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
    
    type UpdateResult =
        | ModelUpdated of Model * Cmd<Message>
        | RecipeSelected of Recipe
    
    let update model msg env =
        match msg with
        | Refresh ->
            ModelUpdated <| ({ model with IsLoading = true }, init |> Cmd.ofReader env)
        | PageRefreshed recommendations ->
            ModelUpdated <| ({ model with Recommendations = recommendations; IsLoading = false }, Cmd.none)
        | GoToRecipeDetail recipe ->
            ModelUpdated <| ({ model with RecipeDetailPageState = Visible <| RecipeDetailPage.initModel recipe }, Cmd.none)
        | HideRecipeDetail ->
            ModelUpdated <| ({ model with RecipeDetailPageState = Hidden }, Cmd.none)
        | RecipeDetailMessage detailMsg ->
            match model.RecipeDetailPageState with
            | Hidden ->
                ModelUpdated <| (model, Cmd.none)
            | Visible detailModel ->
                match detailMsg with
                | RecipeDetailPage.Message.Add -> RecipeSelected detailModel.Recipe
                
    // View
    
    let recipeDetailPage dispatch recipesThatCannotBeAdded foodstuffsInShoppingList = function
        | Hidden -> []
        | Visible (recipeDetailModel: RecipeDetailPage.Model) ->
            let showAdd = not <| Seq.exists (fun r -> r = recipeDetailModel.Recipe) recipesThatCannotBeAdded
            [ RecipeDetailPage.view (RecipeDetailMessage >> dispatch) recipeDetailModel showAdd foodstuffsInShoppingList ]

    let mainContent dispatch isLoading recipes foodstuffInShoppingList =
        Elements.RefreshListPageContent(
            isLoading = isLoading,
            items = recipes,
            itemView = (fun r -> Elements.RecipeCard(actionItems = [], recipe = r, foodstuffInShoppingList = foodstuffInShoppingList)),
            onTapped = (GoToRecipeDetail >> dispatch),
            refresh = (fun () -> dispatch Refresh),
            emptyText = "Go add some ingredients first !",
            rowHeight = 128
        )
    
    let mainPage dispatch isLoading recipes foodstuffsInShoppingList =
        View.ContentPage(
            content = mainContent dispatch isLoading recipes foodstuffsInShoppingList
        )
    
    let view dispatch model recipeIdsThatCannotBeAdded foodstuffsInShoppingList =
        View.NavigationPage(
            title = "Suggestions",
            popped = (fun _ -> dispatch HideRecipeDetail),
            pages = [
                yield dependsOn (model.Recommendations, model.IsLoading) (fun model (rs, isLoading) -> mainPage dispatch isLoading (Seq.toArray rs) foodstuffsInShoppingList)
                yield! recipeDetailPage dispatch recipeIdsThatCannotBeAdded foodstuffsInShoppingList model.RecipeDetailPageState
            ]
        )