namespace SmartRecipes

open AppEnvironment
open System.Net.Mail
open Domain
open Elements
open Fabulous.Core
open Fabulous.DynamicViews
open State
open Xamarin.Forms

[<RequireQualifiedAccess>]
module LoginPage =
    
    type ValidationError =
        | InvalidEmail
    
    type Model = {
        Email: string
        Password: string
        InvalidEmail: bool
    }
    
    type Message = 
        | EmailInputChanged of string 
        | PasswordInputChanged of string 
        | SignInRequested
        | GoToSignUp
    
    let initModel = {
        Email = ""
        Password = ""
        InvalidEmail = false
    }
    
    let toMailAddress s =
        try
            Some <| MailAddress s
        with
        | ex -> None
        
    type UpdateResult =
        | SignUp
        | SignIn of Credentials
        | ModelUpdated of Model * Cmd<Message>
    
    let update msg model =
        match msg with
        | EmailInputChanged email ->
            ModelUpdated ({ model with Email = email }, Cmd.none)
        | PasswordInputChanged password ->
            ModelUpdated ({ model with Password = password }, Cmd.none)
        | SignInRequested ->
            match toMailAddress model.Email with
            | Some m -> SignIn { Email = m; Password = model.Password }
            | None -> ModelUpdated ({ model with InvalidEmail = true }, Cmd.none)
        | GoToSignUp ->
            SignUp
        
    let toErrorMessage = function
        | Api.SignInError.InvalidCredentials -> "Invalid credentials."
        
    let form model dispatch = seq {
        yield Elements.Entry(
            placeholder = "Email",
            value = model.Email,
            callback = (fun s -> dispatch (EmailInputChanged s)),
            ?backgroundColor = if model.InvalidEmail then Some Colors.lightRed else None
        )
        yield Elements.PasswordEntry(
            value = model.Password,
            callback = (fun s -> dispatch (PasswordInputChanged s))
        ) 
        yield Elements.Button(
            text = "Sign in",
            command = (fun () -> dispatch SignInRequested)
        )
        yield Elements.Button(
            text = "Don't have an account yet? Sign up",
            command = (fun () -> dispatch GoToSignUp)
        )
    }
        
    let view signInState (model: Model) dispatch =
        View.ContentPage(
            content = View.StackLayout(
                padding = 32.0,
                margin = 8.0,
                verticalOptions = LayoutOptions.CenterAndExpand,
                children = [
                    yield fix (fun () -> Elements.LargeLabel(text = "Smart Recipes"))
                    yield fix (fun () -> Elements.LargeLabel(text = "Organize cooking"))
                    
                    match signInState with
                    | SignInState.NotRequested ->
                        yield! form model dispatch
                    | SignInState.Loading ->
                        yield View.ActivityIndicator(isRunning = true)
                    | SignInState.Error e -> 
                        yield Elements.LargeLabel(text = toErrorMessage e)
                        yield! form model dispatch
                        
                ]
            )
        )
