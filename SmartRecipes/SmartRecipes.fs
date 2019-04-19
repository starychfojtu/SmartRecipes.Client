namespace SmartRecipes

open Fabulous.Core
open Xamarin.Forms
open Fabulous.DynamicViews
open Library
open Domain

module App =
    open FSharpPlus.Data

    type UnauthorizedPage =     
        | LoginPage
        | SignUpPage
    
    type Page =
        | ShoppingListPage
    
    type UnauthorizedModel = {
        CurrentPage: UnauthorizedPage
        LoginPage: LoginPage.Model
        SignUpPage: SignUpPage.Model
    }
    
    type AuthorizedModel = {
        CurrentPage: Page
        AccessToken: AccessToken
        ShoppingListPage: ShoppingListPage.Model
        ShoppingListRecipePage: ShoppingListRecipePage.Model
    }
    
    type Model = 
        | Unauthorized of UnauthorizedModel
        | Authorized of AuthorizedModel
            
    type Message = 
        | LoginPageMessage of LoginPage.Message
        | SignUpPageMessage of SignUpPage.Message
        | ShoppingListPageMessage of ShoppingListPage.Message
        | ShoppingListRecipeMessage of ShoppingListRecipePage.Message
        | ChangePage of Page
        
//    let api = ProductionApi.instance
    let api = MockedApi.instance

    let initModel = Unauthorized {
        CurrentPage = LoginPage
        LoginPage = LoginPage.initModel api
        SignUpPage = SignUpPage.initModel api
    }

    let init () = initModel, Cmd.none
    
    let initAuthorizedModel accessToken = Authorized {
        CurrentPage = ShoppingListPage
        AccessToken = accessToken
        ShoppingListPage = ShoppingListPage.initModel
        ShoppingListRecipePage = ShoppingListRecipePage.initModel
    }
    
    let initAuthorizedCommand accessToken =
        let shoppingListPageInit = ReaderT.run (ShoppingListPage.init ()) (api, accessToken) |> Cmd.ofAsyncMsg |> Cmd.map ShoppingListPageMessage
        let shoppingListRecipePageInit = ShoppingListRecipePage.init api accessToken |> Cmd.ofAsyncMsg |> Cmd.map ShoppingListRecipeMessage
        Cmd.batch [ shoppingListPageInit; shoppingListRecipePageInit ]
    
    let update msg = function
        | Unauthorized m -> 
            match msg with
            | LoginPageMessage msg ->
                let result = LoginPage.update msg m.LoginPage
                match result with
                | LoginPage.UpdateResult.ModelUpdated (newModel, cmd) -> 
                    Unauthorized { m with LoginPage = newModel }, Cmd.map (LoginPageMessage) cmd
                | LoginPage.UpdateResult.SignedIn token -> 
                    initAuthorizedModel token, initAuthorizedCommand token
                | LoginPage.UpdateResult.SignUp -> 
                    Unauthorized { m with CurrentPage = SignUpPage }, Cmd.none
            | SignUpPageMessage msg ->
                let result = SignUpPage.update msg m.SignUpPage
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
                 let (newModel, cmd) = ShoppingListPage.update m.ShoppingListPage msg
                 Authorized { m with ShoppingListPage = newModel }, Cmd.map (ShoppingListPageMessage) cmd
            | ShoppingListRecipeMessage msg ->
                 let (newModel, cmd) = ShoppingListRecipePage.update m.ShoppingListRecipePage msg
                 Authorized { m with ShoppingListRecipePage = newModel }, Cmd.map (ShoppingListRecipeMessage) cmd
            | _ ->
                failwith "Unhandled message."
              
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
                shoppingListTabPage [ mainPage; recipePage ] |> (appContainer dispatch)
                
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
    //do runner.EnableLiveUpdate()
#endif    



