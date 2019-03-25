namespace SmartRecipes

open System.Diagnostics
open Fabulous.Core
open Fabulous.DynamicViews
open Xamarin.Forms

module App = 
    type EmailError =
        | EmailIsNotValid
        
    type PasswordError =
        | PasswordIsShorterThan of int
      
    type Model = {
        Email: string
        EmailError: EmailError option
        Password: string
        PasswordError: PasswordError option
    }

    type Msg = 
        | EmailInputChanged of string 
        | PasswordInputChanged of string 
        | SignIn
        | GoToSignUp

    let initModel = {
        Email = "";
        EmailError = None;
        Password = "";
        PasswordError = None;
    }

    let init () = initModel, Cmd.none
    
    let update msg model =
        match msg with
        | EmailInputChanged email -> { model with Email = email }, Cmd.none
        | PasswordInputChanged password -> { model with Password = password }, Cmd.none
        | SignIn -> model, Cmd.none
        | GoToSignUp -> model, Cmd.none
        
    let entry value callback =
        View.Entry(
            text = value,
            completed = (fun v -> if v <> value then callback v),
            created = (fun e -> e.Unfocused.Add(fun args -> if value <> e.Text then callback e.Text)),
            verticalOptions = LayoutOptions.FillAndExpand
        )
        
    let passwordEntry value callback = entry value callback |> ViewElementExtensions.isPassword true
        
    let view (model: Model) dispatch =
        View.ContentPage(
            content = View.StackLayout(
                padding = 32.0,
                margin = 8.0,
                verticalOptions = LayoutOptions.CenterAndExpand,
                children = [
                    View.Label(text = "Smart Recipes", horizontalTextAlignment = TextAlignment.Center)
                    View.Label(text = "Organize cooking", horizontalTextAlignment = TextAlignment.Center)
                    entry model.Email (fun s -> dispatch (EmailInputChanged s))
                    passwordEntry model.Password (fun s -> dispatch (PasswordInputChanged s))
                    View.Button(text = "Sign in", verticalOptions = LayoutOptions.FillAndExpand, command = (fun () -> dispatch SignIn))
                    View.Button(text = "Don't have an account yet? Sign up", verticalOptions = LayoutOptions.FillAndExpand, command = (fun () -> dispatch GoToSignUp))
                ]
            )
        )

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



