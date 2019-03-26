namespace SmartRecipes

open Fabulous.Core
open Xamarin.Forms

module App =

    type Page =
        | LoginPage
        | ShoppingListPage
    
    type Model = {
         CurrentPage: Page
         LoginPage: LoginPage.Model
         ShoppingListPage: ShoppingListPage.Model
    }
            
    type Message = 
        | LoginPageMessage of LoginPage.Message
        | ShoppingListPageMessage of ShoppingListPage.Message

    let initModel = {
        CurrentPage = LoginPage
        LoginPage = LoginPage.initModel
        ShoppingListPage = ShoppingListPage.initModel
    }

    let init () = initModel, Cmd.none
    
    let update msg model =
        match msg with
        | LoginPageMessage msg ->
             let result = LoginPage.update msg model.LoginPage
             match result with
             | LoginPage.UpdateResult.ModelUpdated (m, cmd) -> { model with LoginPage = m }, Cmd.map (LoginPageMessage) cmd
             | LoginPage.UpdateResult.SignedIn -> { model with CurrentPage = ShoppingListPage }, Cmd.none // TODO: cmd for init
             | LoginPage.UpdateResult.SignUp -> failwith "Not implemented"
        | ShoppingListPageMessage msg ->
             let (newModel, cmd) = ShoppingListPage.update model.ShoppingListPage msg
             { model with ShoppingListPage = newModel }, Cmd.map (ShoppingListPageMessage) cmd
         

    let view model dispatch =    
        match model.CurrentPage with
        | LoginPage -> LoginPage.view model.LoginPage (LoginPageMessage >> dispatch)
        | ShoppingListPage -> ShoppingListPage.view model.ShoppingListPage (ShoppingListPageMessage >> dispatch)

    let program = Program.mkProgram init update view

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



