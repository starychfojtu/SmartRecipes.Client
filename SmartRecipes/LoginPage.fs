namespace SmartRecipes

open Fabulous.Core
open Fabulous.DynamicViews
open Xamarin.Forms

[<RequireQualifiedAccess>]
module LoginPage =
    open Domain
    
    type Model = {
        Email: string
        EmailError: string option
        Password: string
        PasswordError: string option
        Error: string option
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
        EmailError = None
        Password = ""
        PasswordError = None
        Error = None
        IsLoading = false
    }
    
    let signIn (model: Model) =
        let message =  async {
            let! response = Api.sendSignInRequest model.Email model.Password
            return SignInResponseRecieved response
        }
        ({ model with IsLoading = true }, message |> Cmd.ofAsyncMsg)
         
    let processError (error: Api.SignInError) (model: Model) =
        ({ model with Error = Some error.Error; EmailError = error.EmailError; PasswordError = error.PasswordError }, Cmd.none)
        
    type UpdateResult =
        | SignUp
        | SignedIn of AccessToken
        | ModelUpdated of Model * Cmd<Message>
    
    let update msg (model: Model) =
        match msg with
        | EmailInputChanged email -> ModelUpdated ({ model with Email = email }, Cmd.none)
        | PasswordInputChanged password -> ModelUpdated ({ model with Password = password }, Cmd.none)
        | SignInRequested -> signIn model |> ModelUpdated
        | SignInResponseRecieved response ->
            match response with
            | Ok r -> SignedIn r.AccessToken
            | Error e -> processError e model |> ModelUpdated
        | GoToSignUp -> SignUp
        
    let view (model: Model) dispatch =
        View.ContentPage(
            content = View.StackLayout(
                padding = 32.0,
                margin = 8.0,
                verticalOptions = LayoutOptions.CenterAndExpand,
                children = [
                    yield View.Label(text = "Smart Recipes", horizontalTextAlignment = TextAlignment.Center)
                    yield View.Label(text = "Organize cooking", horizontalTextAlignment = TextAlignment.Center)
                    if Option.isSome model.Error then yield  View.Label(text = Option.get model.Error)
                    for e in Elements.entry model.Email model.EmailError (fun s -> dispatch (EmailInputChanged s)) do yield e
                    for e in Elements.passwordEntry model.Password model.PasswordError (fun s -> dispatch (PasswordInputChanged s)) do yield e
                    yield View.Button(text = "Sign in", verticalOptions = LayoutOptions.FillAndExpand, command = (fun () -> dispatch SignInRequested))
                    yield View.Button(text = "Don't have an account yet? Sign up", verticalOptions = LayoutOptions.FillAndExpand, command = (fun () -> dispatch GoToSignUp))
                ]
            )
        )
