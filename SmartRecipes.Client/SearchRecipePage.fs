namespace SmartRecipes

open Elements
open AppEnvironment
open Domain
open FSharpPlus
open FSharpPlus.Data
open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms

module SearchRecipePage =
    
    type Model = {
        Term: string
        Results: Recipe seq
    }
    
    type Message =
        | TermChanged of string
        | NewResults of Recipe seq
        | SelectRecipe of Recipe
        
    let initModel = {
        Term = ""
        Results = Seq.empty
    }

    let private searchRecipes term = ReaderT(fun env -> 
        env.Api.SearchRecipes { Term = term })
    
    let private search term =
        searchRecipes term |> ReaderT.map (fun r -> NewResults r.Recipes)
    
    type UpdateResult = 
        | ModelUpdated of Model * Cmd<Message>
        | RecipeSelected of Recipe
            
    let update model message env =
        match message with
        | TermChanged term ->
            ModelUpdated ({ model with Term = term }, search term |> Cmd.ofReader env)
        | NewResults results ->
            ModelUpdated ({ model with Results = results }, Cmd.none)
        | SelectRecipe recipe ->
            RecipeSelected recipe
            
    let private searchBar dispatch =
        let textChanged (args: TextChangedEventArgs) = TermChanged args.NewTextValue |> dispatch
        View.SearchBar(textChanged = debounce 500 textChanged)
        
    let private resultTable dispatch recipes foodstuffsInShoppingList =
        Elements.List(
            items = recipes,
            itemView = (fun r -> Elements.RecipeCard(actionItems = [], recipe = r, foodstuffInShoppingList = foodstuffsInShoppingList)),
            onTapped = (SelectRecipe >> dispatch),
            refresh = ListRefresh.No,
            rowHeight = 124
        )
            
    let view dispatch model ignoredRecipes foodstuffsInShoppingList =
        let recipes = Seq.except ignoredRecipes model.Results
        View.ContentPage(
            content = View.StackLayout(
                children = [
                    yield fix (fun () -> searchBar dispatch)
                    yield dependsOn recipes (fun model rs -> resultTable dispatch (Seq.toList rs) foodstuffsInShoppingList)
                ]                 
            )
        )

