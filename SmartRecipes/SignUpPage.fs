namespace SmartRecipes

open Elements
open Fabulous.Core
open Fabulous.DynamicViews
open Xamarin.Forms

[<RequireQualifiedAccess>]
module SignUpPage =
    open Api
    open Domain
    open FSharpPlus.Data
    open AppEnvironment
    open FSharpPlus
    
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
    
    let trySignUp model (env: UnauthorizedEnvironment) =
        env.Api.SignUp { Email = model.Email; Password = model.Password }
        |> Async.map SignUpResponseRecieved
        |> Cmd.ofAsyncMsg
         
    let processError error model =
        ({ model with Error = Some error }, Cmd.none)
        
    type UpdateResult =
        | SignedUp of Account
        | SignIn
        | ModelUpdated of Model * Cmd<Message>
    
    let update msg model env =
        match msg with
        | EmailChanged email -> ModelUpdated ({ model with Email = email }, Cmd.none)
        | PasswordInputChanged password -> ModelUpdated ({ model with Password = password }, Cmd.none)
        | SignUpRequested -> ModelUpdated({ model with IsLoading = true }, trySignUp model env)
        | SignUpResponseRecieved response ->
            match response with
            | Ok r -> SignedUp r.Account
            | Error e -> processError e model |> ModelUpdated
        | GoToSignIn -> SignIn

    let mapInvalidParameters f = function
        | Api.SignUpError.AccountAlreadyExists -> None
        | Api.SignUpError.InvalidSignUpParameters e -> f e
        
    let emailEntry dispatch email error =
        let error = Option.bind (mapInvalidParameters (fun e -> e.EmailError)) error
        Elements.validatableEntry "Email" email error (fun s -> dispatch (EmailChanged s))

    let passwordEntry dispatch password error =
        let error = Option.bind (mapInvalidParameters (fun e -> e.PasswordError)) error
        Elements.passwordValidatableEntry password error (fun s -> dispatch (PasswordInputChanged s))
    
    let toErrorMessage = function
        | Api.SignUpError.AccountAlreadyExists -> Some "Account already exists."
        | Api.SignUpError.InvalidSignUpParameters _ -> None
        
    let errorEntry error =
        Option.bind (toErrorMessage) error
        |> Option.map (fun e -> View.Label(text = e))
        |> Option.toArray
        
    let view (model: Model) dispatch =
        View.ContentPage(
            content = View.StackLayout(
                padding = 32.0,
                margin = 8.0,
                verticalOptions = LayoutOptions.CenterAndExpand,
                children = [
                    yield fix (fun () -> View.Label(text = "Smart Recipes", horizontalTextAlignment = TextAlignment.Center))
                    yield fix (fun () -> View.Label(text = "Join organized cooks !", horizontalTextAlignment = TextAlignment.Center))
                    yield! dependsOn model.Error (fun model -> errorEntry)
                    yield Elements.Entry(
                        placeholder = "First name",
                        value = "",
                        callback = (fun _ -> ())
                    )
                    yield Elements.Entry(
                        placeholder = "Last name",
                        value = "",
                        callback = (fun _ -> ())
                    )
                    yield! dependsOn (model.Email, model.Error) (fun model (email, error) -> emailEntry dispatch email error)
                    yield! dependsOn (model.Password, model.Error) (fun model (password, error) -> passwordEntry dispatch password error)
                    yield fix (fun () -> View.Button(text = "Sign up", verticalOptions = LayoutOptions.FillAndExpand, command = (fun () -> dispatch SignUpRequested)))
                    yield fix (fun () -> View.Button(text = "Already have an account? Sign in", verticalOptions = LayoutOptions.FillAndExpand, command = (fun () -> dispatch GoToSignIn)))
                ]
            )
        )
