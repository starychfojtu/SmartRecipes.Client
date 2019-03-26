namespace SmartRecipes

open FSharp.Data
open Newtonsoft.Json

module Api =
    open System

    type ApiError = {
        Message: string
        ParameterErrors: Map<string, string>
    }
    
    type private ApiErrorJson = JsonProvider<""" {
        "message": "Invalid parameters",
        "parameterErrors": [{
            "parameter": "Email",
            "message": "Must containt @"
        }]
    } """>
    
    let private baseUri = System.Uri("https://smart-recipes.herokuapp.com/")
    
    let private parseApiError value =
        let json = ApiErrorJson.Parse value
        let parameterErrors = json.ParameterErrors |> Array.map (fun (e: ApiErrorJson.ParameterError) -> (e.Parameter, e.Message)) |> Map.ofSeq
        { Message = json.Message; ParameterErrors = parameterErrors }
            
    let private sendRequest (path: string) body parseSuccess parseError =
        async {
            do! Async.SwitchToThreadPool ()
            let! response = Http.AsyncRequest(Uri(baseUri, path).ToString(), httpMethod = "POST", body = TextRequest body)
            let body = response.Body.ToString();
            return
                if response.StatusCode = HttpStatusCodes.OK
                then body |> parseSuccess |> Ok
                else body |> parseApiError |> parseError |> Error
        }
        
    // Sign in
        
    type private SignInRequest = {
        Email: string
        Password: string
    }
    
    type SignInResponse = {
        AccessToken: string
        AccountId: string
        ExpirationUtc: DateTime
    }
    
    type SignInError = {
        EmailError: string option
        PasswordError: string option
        Error: string
    }
    
    type private SignInResponseJson = JsonProvider<""" {
        "value": "random-token-hash",
        "accountId": "guid",
        "expirationUtc": "2019-03-25T23:49:18"
    } """>
    
    let private parseSignInResponse value =
        let json = SignInResponseJson.Parse value
        { AccessToken = json.Value; AccountId = json.AccountId; ExpirationUtc = json.ExpirationUtc  }
        
    let private parseSignInError apiError =
        let emailError = Map.tryFind "Email" apiError.ParameterErrors
        let passwordError = Map.tryFind "Password" apiError.ParameterErrors
        { EmailError = emailError; PasswordError = passwordError;  Error = apiError.Message }

    let sendSignInRequest email password: Async<Result<SignInResponse, SignInError>> =
//        let body = JsonConvert.SerializeObject({ Email = email; Password = password; })
//        sendRequest "/signIn" body parseSignInResponse parseSignInError
        if String.IsNullOrEmpty email
            then async { return Ok { AccessToken = "fake"; AccountId = "fake"; ExpirationUtc = DateTime.UtcNow } }
            else async { return Error { Error = "Something is wrong"; EmailError = Some "Nope"; PasswordError = Some "Also nope" } }
        
        
