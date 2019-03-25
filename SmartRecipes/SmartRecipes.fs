namespace SmartRecipes

open Fabulous.Core
open Xamarin.Forms

module App =
    type Page =
        | LoginPage
    
    type Model = {
        CurrentPage: Page
        LoginPage: LoginPage.Model
    }

    type Message = 
        | LoginPageMessage of LoginPage.Message

    let initModel = {
        CurrentPage = LoginPage
        LoginPage = LoginPage.initModel
    }

    let init () = initModel, Cmd.none
    
    let update msg model =
        match msg with
        | LoginPageMessage loginPageMsg ->
             let (newModel, cmd) = LoginPage.update loginPageMsg model.LoginPage
             ({ model with LoginPage = newModel }, cmd)

    let view model dispatch =    
        match model.CurrentPage with
        | LoginPage -> LoginPage.view model.LoginPage (fun msg -> dispatch (LoginPageMessage msg))

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



