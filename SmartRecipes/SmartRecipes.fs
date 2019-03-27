namespace SmartRecipes

open Fabulous.Core
open Xamarin.Forms
open Fabulous.DynamicViews
open Library
open Domain

module App =

    type UnauthorizedPage =     
        | LoginPage
    
    type Page =
        | ShoppingListPage
    
    type UnauthorizedModel = {
        CurrentPage: UnauthorizedPage
        LoginPage: LoginPage.Model
    }
    
    type AuthorizedModel = {
        CurrentPage: Page
        AccessToken: AccessToken
        ShoppingListPage: ShoppingListPage.Model
    }
    
    type Model = 
        | Unauthorized of UnauthorizedModel
        | Authorized of AuthorizedModel
            
    type Message = 
        | LoginPageMessage of LoginPage.Message
        | ShoppingListPageMessage of ShoppingListPage.Message
        | ChangePage of Page

    let initModel = Unauthorized {
        CurrentPage = LoginPage
        LoginPage = LoginPage.initModel
    }

    let init () = initModel, Cmd.none
    
    let initAuthorizedModel accessToken = Authorized {
        CurrentPage = ShoppingListPage
        AccessToken = accessToken
        ShoppingListPage = ShoppingListPage.initModel
    }
    
    let initAuthorizedCommand accessToken =
        ShoppingListPage.init accessToken |> Cmd.ofAsyncMsg |> Cmd.map ShoppingListPageMessage
    
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
                    failwith "Not implemented"
            | _ ->
                failwith "Unhandled message."
        | Authorized m ->     
            match msg with
            | ChangePage page ->
                (Authorized m, Cmd.none)
            | ShoppingListPageMessage msg ->
                 let (newModel, cmd) = ShoppingListPage.update m.ShoppingListPage msg
                 Authorized { m with ShoppingListPage = newModel }, Cmd.map (ShoppingListPageMessage) cmd
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
        
    let shoppingListTabPage shoppingListPage =     
         View.TabbedPage(
            title = "Shopping list",
            children = [ shoppingListPage ]
        )

    let view dispatch = function  
        | Unauthorized m ->   
            match m.CurrentPage with
            | LoginPage -> 
                LoginPage.view m.LoginPage (LoginPageMessage >> dispatch)
        | Authorized m -> 
            match m.CurrentPage with
            | ShoppingListPage -> 
                ShoppingListPage.view (ShoppingListPageMessage >> dispatch) m.ShoppingListPage 
                |> shoppingListTabPage 
                |> (appContainer dispatch)
                
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



