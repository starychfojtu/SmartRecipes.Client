namespace SmartRecipes

open Elements
open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms

[<RequireQualifiedAccess>]
module LoginPage =
    open AppEnvironment
    open Domain
    
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
        Email = "josef.starychfojtu@gmail.com"
        Password = ""
        Error = None
        IsLoading = false
    }
    
    let signIn model (env: UnauthorizedEnvironment) =
        env.Api.SignIn { Email = model.Email; Password = model.Password }
        |> Async.map SignInResponseRecieved
        |> Cmd.ofAsyncMsg
         
    let processError (error: Api.SignInError) (model: Model) =
        ({ model with Error = Some error; IsLoading = false }, Cmd.none)
        
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
        Option.map (fun e -> (Elements.LargeLabel(text = toErrorMessage e))) >> Option.toArray
        
    let view (model: Model) dispatch =
        View.ContentPage(
            content = View.StackLayout(
                padding = Thickness 32.0,
                margin = Thickness 8.0,
                verticalOptions = LayoutOptions.CenterAndExpand,
                children = [
                    yield fix (fun () -> Elements.LargeLabel(text = "Smart Recipes"))
                    yield fix (fun () -> Elements.LargeLabel(text = "Organize cooking"))
                    
                    if (model.IsLoading)
                    then
                        yield View.ActivityIndicator(isRunning = true)
                    else
                        yield! dependsOn model.Error (fun _ -> errorLabel)
                        yield Elements.Entry(
                            placeholder = "Email",
                            value = model.Email,
                            callback = (fun s -> dispatch (EmailInputChanged s))
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
                ]
            )
        )
