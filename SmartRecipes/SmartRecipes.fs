namespace SmartRecipes

open Fabulous.Core
open Xamarin.Forms
open Fabulous.DynamicViews
open Library
open Domain

module App =
    open AppEnvironment
    open FSharpPlus
    open FSharpPlus
    open FSharpPlus.Data
    open Fabulous.Core
    open Library

    type UnauthorizedPage =     
        | LoginPage
        | SignUpPage
    
    type Page =
        | ShoppingListPage
    
    type UnauthorizedModel = {
        CurrentPage: UnauthorizedPage
        LoginPage: LoginPage.Model
        SignUpPage: SignUpPage.Model
        Environment: Environment
    }
    
    type AuthorizedModel = {
        CurrentPage: Page
        ShoppingListPage: ShoppingListPage.Model
        ShoppingListRecipePage: ShoppingListRecipePage.Model
        RecipeRecommendationPage: RecipeRecommendationPage.Model
        Environment: AuthorizedEnvironment
    }
    
    type Model = 
        | Unauthorized of UnauthorizedModel
        | Authorized of AuthorizedModel
            
    type Message = 
        | LoginPageMessage of LoginPage.Message
        | SignUpPageMessage of SignUpPage.Message
        | ShoppingListPageMessage of ShoppingListPage.Message
        | ShoppingListRecipeMessage of ShoppingListRecipePage.Message
        | RecipeRecommendationPageMessage of RecipeRecommendationPage.Message
        | ChangePage of Page

    let initModel (env: Environment) = Unauthorized {
        CurrentPage = LoginPage
        LoginPage = LoginPage.initModel
        SignUpPage = SignUpPage.initModel
        Environment = env
    }
    
    let prodEnvironment = {
        Unauthorized = { Api = ProductionApi.unauthorized }
        GetAuthorized = fun token -> { Api = ProductionApi.authorized token }
    }
    
    let devEnvironment = {
        Unauthorized = { Api = MockedApi.unauthorized }
        GetAuthorized = fun token -> { Api = MockedApi.authorized token }
    }

    let init () = initModel prodEnvironment, Cmd.none
    
    let initAuthorizedModel env = Authorized {
        CurrentPage = ShoppingListPage
        ShoppingListPage = ShoppingListPage.initModel
        ShoppingListRecipePage = ShoppingListRecipePage.initModel
        RecipeRecommendationPage = RecipeRecommendationPage.initModel
        Environment = env
    }

    let initAuthorizedCommand env =
        let shoppingListPageInit = ShoppingListPage.init |> Cmd.ofReader env
        let shoppingListRecipePageInit = ShoppingListRecipePage.init |> Cmd.ofReader env
        let recipeRecommendationPageInit = RecipeRecommendationPage.init |> Cmd.ofReader env
        Cmd.batch [
            shoppingListPageInit |> Cmd.map ShoppingListPageMessage
            shoppingListRecipePageInit |> Cmd.map ShoppingListRecipeMessage 
            recipeRecommendationPageInit |> Cmd.map RecipeRecommendationPageMessage 
        ]
    
    let updatePage msg = function
        | Unauthorized m -> 
            match msg with
            | LoginPageMessage msg ->
                let result = LoginPage.update msg m.LoginPage m.Environment.Unauthorized
                match result with
                | LoginPage.UpdateResult.ModelUpdated (newModel, cmd) -> 
                    Unauthorized { m with LoginPage = newModel }, Cmd.map (LoginPageMessage) cmd
                | LoginPage.UpdateResult.SignedIn token ->
                    let authorizedEnv = m.Environment.GetAuthorized token
                    initAuthorizedModel authorizedEnv, initAuthorizedCommand authorizedEnv
                | LoginPage.UpdateResult.SignUp -> 
                    Unauthorized { m with CurrentPage = SignUpPage }, Cmd.none
            | SignUpPageMessage msg ->
                let result = SignUpPage.update msg m.SignUpPage m.Environment.Unauthorized
                match result with
                | SignUpPage.UpdateResult.ModelUpdated (newModel, cmd) -> 
                    Unauthorized { m with SignUpPage = newModel }, Cmd.map (SignUpPageMessage) cmd
                | SignUpPage.UpdateResult.SignedUp account ->
                    let loginPageModel = { m.LoginPage with Email = account.Email.Address }
                    Unauthorized { m with CurrentPage = LoginPage; LoginPage = loginPageModel }, Cmd.none
                | SignUpPage.UpdateResult.SignIn -> 
                    Unauthorized { m with CurrentPage = LoginPage }, Cmd.none
            | _ ->
                failwith "Unhandled message."
        | Authorized m ->     
            match msg with
            | ChangePage page ->
                (Authorized m, Cmd.none)
            | ShoppingListPageMessage msg ->
                 let (newModel, cmd) = ShoppingListPage.update m.ShoppingListPage msg m.Environment
                 Authorized { m with ShoppingListPage = newModel }, Cmd.map (ShoppingListPageMessage) cmd
            | ShoppingListRecipeMessage msg ->
                 let (newShoppingListPageModel, cmd) = ShoppingListRecipePage.update m.ShoppingListRecipePage msg m.Environment
                 Authorized { m with ShoppingListRecipePage = newShoppingListPageModel }, Cmd.map (ShoppingListRecipeMessage) cmd
            | RecipeRecommendationPageMessage msg ->
                let (newModel, cmd) = RecipeRecommendationPage.update m.RecipeRecommendationPage msg m.Environment
                Authorized { m with RecipeRecommendationPage = newModel }, Cmd.map (RecipeRecommendationPageMessage) cmd
            | _ ->
                failwith "Unhandled message."
                
    let update msg model =
        let (newModel, cmd) = updatePage msg model
        
        let refreshRecommendationsMsg =
            match msg with
            | ShoppingListRecipeMessage (ShoppingListRecipePage.Message.ItemsChanged _)
            | ShoppingListPageMessage (ShoppingListPage.Message.ShoppingListChanged _) ->    
                RecipeRecommendationPage.Message.Refresh |> RecipeRecommendationPageMessage |> Some
            | _ -> None
            
        let newCmd =  Cmd.batch [
            cmd;
            Cmd.ofMsgOption refreshRecommendationsMsg
        ]
            
        (newModel, newCmd)

    let navigationPages = [| (ShoppingListPage, "ShoppingList") |]
             
    let appContainer dispatch detail =
        View.MasterDetailPage(
            detail = detail,
            master = View.ContentPage(
                title = "Smart Recipes",
                content = View.ListView(
                    items = Seq.map (fun (_, text) -> View.Label text) navigationPages,
                    itemTapped = (fun index -> Array.get navigationPages index |> first |> ChangePage |> dispatch)
                )
            )
        )
        
    let shoppingListTabPage chidren =     
         View.TabbedPage(
            title = "Shopping list",
            children = chidren
        )

    let view dispatch = function  
        | Unauthorized m ->   
            match m.CurrentPage with
            | LoginPage -> 
                LoginPage.view m.LoginPage (LoginPageMessage >> dispatch)
            | SignUpPage -> 
                SignUpPage.view m.SignUpPage (SignUpPageMessage >> dispatch)
        | Authorized m -> 
            match m.CurrentPage with
            | ShoppingListPage ->
                let mainPage = ShoppingListPage.view (ShoppingListPageMessage >> dispatch) m.ShoppingListPage
                let recipePage = ShoppingListRecipePage.view (ShoppingListRecipeMessage >> dispatch) m.ShoppingListRecipePage
                let recommendationPage = RecipeRecommendationPage.view (RecipeRecommendationPageMessage >> dispatch) m.RecipeRecommendationPage
                shoppingListTabPage [ mainPage; recipePage; recommendationPage ] |> (appContainer dispatch)
                
    let programView model dispatch = view dispatch model    
    let program = Program.mkProgram init update programView

type App () as app = 
    inherit Application ()

    let runner = 
        App.program
#if DEBUG
        |> Program.withConsoleTrace
#endif
        |> Program.runWithDynamicView app

#if DEBUG
    // Uncomment this line to enable live update in debug mode. 
    // See https://fsprojects.github.io/Fabulous/tools.html for further  instructions.
    // do runner.EnableLiveUpdate()
#endif    



