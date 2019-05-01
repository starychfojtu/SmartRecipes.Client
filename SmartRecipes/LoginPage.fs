namespace SmartRecipes

open Fabulous.Core
open Fabulous.DynamicViews
open Xamarin.Forms

[<RequireQualifiedAccess>]
module LoginPage =
    open AppEnvironment
    open Domain
    open FSharpx.Control
    
    type Model = {
        Email: string
        Password: string
        Error: Api.SignInError option
        IsLoading: bool
    }
    
    type Message = 
        | EmailInputChanged of string 
        | PasswordInputChanged of string 
        | SignInRequested
        | SignInResponseRecieved of Result<Api.SignInResponse, Api.SignInError>
        | GoToSignUp
    
    let initModel = {
        Email = ""
        Password = ""
        Error = None
        IsLoading = false
    }
    
    let signIn model (env: UnauthorizedEnvironment) =
        env.Api.SignIn { Email = model.Email; Password = model.Password }
        |> Async.map SignInResponseRecieved
        |> Cmd.ofAsyncMsg
         
    let processError (error: Api.SignInError) (model: Model) =
        ({ model with Error = Some error }, Cmd.none)
        
    type UpdateResult =
        | SignUp
        | SignedIn of AccessToken
        | ModelUpdated of Model * Cmd<Message>
    
    let update msg model env =
        match msg with
        | EmailInputChanged email -> ModelUpdated ({ model with Email = email }, Cmd.none)
        | PasswordInputChanged password -> ModelUpdated ({ model with Password = password }, Cmd.none)
        | SignInRequested -> ModelUpdated ({ model with IsLoading = true }, signIn model env)
        | SignInResponseRecieved response ->
            match response with
            | Result.Ok r -> SignedIn r.AccessToken
            | Result.Error e -> processError e model |> ModelUpdated
        | GoToSignUp -> SignUp
        
    let toErrorMessage = function
        | Api.SignInError.InvalidCredentials -> "Invalid credentials."
        
    let errorLabel =
        Option.map (fun e -> (View.Label(text = toErrorMessage e))) >> Option.toArray
        
    let view (model: Model) dispatch =
        View.ContentPage(
            content = View.StackLayout(
                padding = 32.0,
                margin = 8.0,
                verticalOptions = LayoutOptions.CenterAndExpand,
                children = [
                    yield fix (fun () -> View.Label(text = "Smart Recipes", horizontalTextAlignment = TextAlignment.Center))
                    yield fix (fun () -> View.Label(text = "Organize cooking", horizontalTextAlignment = TextAlignment.Center))
                    yield! dependsOn model.Error (fun model -> errorLabel)
                    yield Elements.entry model.Email (fun s -> dispatch (EmailInputChanged s))
                    yield Elements.passwordEntry model.Password (fun s -> dispatch (PasswordInputChanged s))
                    yield View.Button(text = "Sign in", verticalOptions = LayoutOptions.FillAndExpand, command = (fun () -> dispatch SignInRequested))
                    yield View.Button(text = "Don't have an account yet? Sign up", verticalOptions = LayoutOptions.FillAndExpand, command = (fun () -> dispatch GoToSignUp))
                ]
            )
        )
