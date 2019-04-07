namespace SmartRecipes

open Fabulous.Core
open Fabulous.DynamicViews
open Xamarin.Forms

[<RequireQualifiedAccess>]
module SignUpPage =
    open Domain
    
    type Model = {
        Email: string
        Password: string
        Error: Api.SignUpError option
        IsLoading: bool
    }
    
    type Message = 
        | EmailChanged of string 
        | PasswordInputChanged of string 
        | SignUpRequested
        | SignUpResponseRecieved of Result<Api.SignUpResponse, Api.SignUpError>
        | GoToSignIn
    
    let initModel = {
        Email = ""
        Password = ""
        Error = None
        IsLoading = false
    }
    
    let signUp (model: Model) =
        let message =  async {
            let! response = Api.sendSignUpRequest model.Email model.Password
            return SignUpResponseRecieved response
        }
        ({ model with IsLoading = true }, message |> Cmd.ofAsyncMsg)
         
    let processError (error: Api.SignUpError) (model: Model) =
        ({ model with Error = Some error }, Cmd.none)
        
    type UpdateResult =
        | SignedUp of Account
        | SignIn
        | ModelUpdated of Model * Cmd<Message>
    
    let update msg (model: Model) =
        match msg with
        | EmailChanged email -> ModelUpdated ({ model with Email = email }, Cmd.none)
        | PasswordInputChanged password -> ModelUpdated ({ model with Password = password }, Cmd.none)
        | SignUpRequested -> signUp model |> ModelUpdated
        | SignUpResponseRecieved response ->
            match response with
            | Ok r -> SignedUp r.Account
            | Error e -> processError e model |> ModelUpdated
        | GoToSignIn -> SignIn

    let mapInvalidParameters f = function
        | Api.SignUpError.AccountAlreadyExists -> None
        | Api.SignUpError.InvalidSignUpParameters e -> f e
        
    let emailEntry dispatch model =
        let error = Option.bind (mapInvalidParameters (fun e -> e.EmailError)) model.Error
        Elements.validatableEntry model.Email error (fun s -> dispatch (EmailChanged s))

    let passwordEntry dispatch model =
        let error = Option.bind (mapInvalidParameters (fun e -> e.PasswordError)) model.Error
        Elements.passwordValidatableEntry model.Password error (fun s -> dispatch (PasswordInputChanged s))
    
    let toErrorMessage = function
        | Api.SignUpError.AccountAlreadyExists -> Some "Account already exists."
        | Api.SignUpError.InvalidSignUpParameters _ -> None
        
    let errorEntry model =
        Option.bind (toErrorMessage) model.Error
        |> Option.map (fun e -> View.Label(text = e))
        
    let view (model: Model) dispatch =
        View.ContentPage(
            content = View.StackLayout(
                padding = 32.0,
                margin = 8.0,
                verticalOptions = LayoutOptions.CenterAndExpand,
                children = [
                    // This positioning is a hotfix of bug in Fabulous.
                    for e in emailEntry dispatch model do yield e
                    for e in passwordEntry dispatch model do yield e
                    yield View.Label(text = "Smart Recipes", horizontalTextAlignment = TextAlignment.Center)
                    yield View.Label(text = "Organize cooking", horizontalTextAlignment = TextAlignment.Center)
                    for e in errorEntry model |> Option.toArray do yield e
                    yield View.Button(text = "Sign up", verticalOptions = LayoutOptions.FillAndExpand, command = (fun () -> dispatch SignUpRequested))
                    yield View.Button(text = "Already have an account? Sign in", verticalOptions = LayoutOptions.FillAndExpand, command = (fun () -> dispatch GoToSignIn))
                ]
            )
        )
