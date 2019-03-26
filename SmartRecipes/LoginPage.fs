namespace SmartRecipes

open Fabulous.Core
open Fabulous.DynamicViews
open Xamarin.Forms
open Api

module LoginPage =
    
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
        | SignIn
        | SignInResponseRecieved of Result<SignInResponse, SignInError>
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
            let! response = sendSignInRequest model.Email model.Password
            return SignInResponseRecieved response
        }
        ({ model with IsLoading = true }, message |> Cmd.ofAsyncMsg )
         
    let processError error model =
        (model, Cmd.none)
    
    let update msg (model: Model) =
        match msg with
        | EmailInputChanged email -> { model with Email = email }, Cmd.none
        | PasswordInputChanged password -> { model with Password = password }, Cmd.none
        | SignIn -> signIn model
        | SignInResponseRecieved response ->
            match response with
            | Ok _ -> model, Cmd.none
            | Error e -> processError e model
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
