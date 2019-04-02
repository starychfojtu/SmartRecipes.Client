namespace SmartRecipes

open Fabulous.Core
open Fabulous.DynamicViews
open Xamarin.Forms

[<RequireQualifiedAccess>]
module SignUpPage =
    
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
        | SignUpRequested
        | SignUpResponseRecieved of Result<Api.SignInResponse, Api.SignInError>
        | GoToSignIn
    
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
            return SignUpResponseRecieved response
        }
        ({ model with IsLoading = true }, message |> Cmd.ofAsyncMsg)
         
    let processError (error: Api.SignInError) (model: Model) =
        ({ model with Error = Some error.Error; EmailError = error.EmailError; PasswordError = error.PasswordError }, Cmd.none)
        
    type UpdateResult =
        | SignedUp
        | SignIn
        | ModelUpdated of Model * Cmd<Message>
    
    let update msg (model: Model) =
        match msg with
        | EmailInputChanged email -> ModelUpdated ({ model with Email = email }, Cmd.none)
        | PasswordInputChanged password -> ModelUpdated ({ model with Password = password }, Cmd.none)
        | SignUpRequested -> signIn model |> ModelUpdated
        | SignUpResponseRecieved response ->
            match response with
            | Ok r -> SignedUp
            | Error e -> processError e model |> ModelUpdated
        | GoToSignIn -> SignIn
        
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
                    yield View.Button(text = "Sign up", verticalOptions = LayoutOptions.FillAndExpand, command = (fun () -> dispatch SignUpRequested))
                    yield View.Button(text = "Already have an account? Sign in", verticalOptions = LayoutOptions.FillAndExpand, command = (fun () -> dispatch GoToSignIn))
                ]
            )
        )
