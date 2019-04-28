namespace SmartRecipes

[<RequireQualifiedAccess>]
module ProductionApi =
    open System.IO
    open System.Net
    open Domain
    open FSharp.Data
    open FSharpx.Control
    open Newtonsoft.Json
    open System
    open Library
    open Api
    open FSharp.Json

    type ApiError = {
        Message: string
        ParameterErrors: Map<string, string>
    }
    
    type ApiParameterErrorJson = {
        Parameter: string
        Message: string
    }
    
    type ApiErrorJson = {
        Message: string
        ParameterErrors: ApiParameterErrorJson seq
    }
    
    let private unhandledError () =
        failwith "Unhandled API error."

    let private baseUri = 
        System.Uri("https://smart-recipes.herokuapp.com/")
    
    let private parseApiError value =
        let error = Json.deserialize<ApiErrorJson> value
        let parameterErrors = error.ParameterErrors |> Seq.map (fun e -> (e.Parameter, e.Message)) |> Map.ofSeq
        { ApiError.Message = error.Message; ParameterErrors = parameterErrors }
            
    let private sendRequest httpMethod (path: string) query body accessToken parseSuccess parseError: Async<Result<'a, 'b>> =
        async {
            try
                let authorization = Option.map (fun (t: AccessToken) -> t.Value) accessToken
                let textRequest = Option.map (TextRequest) body
                let! response =
                    Http.AsyncRequest(
                        Uri(baseUri, path).ToString(),
                        query = query,
                        httpMethod = httpMethod,
                        ?body = textRequest,
                        headers = seq {
                            if Option.isSome authorization then
                                yield ("authorization", Option.get authorization)
                            yield ("Content-Type", "application/json")
                        }
                    )
                
                return
                    match response.Body with
                    | Text t -> t |> parseSuccess |> Result.Ok
                    | _ -> failwith "Invalid response body."
            with
            | :? WebException as ex ->
                use sr = new StreamReader(ex.Response.GetResponseStream())
                return sr.ReadToEnd () |> parseApiError |> parseError |> Result.Error
        }
        
    let private get path accessToken parseSuccess parseError: Async<Result<'a, 'b>> =
        sendRequest HttpMethod.Get path List.empty None accessToken parseSuccess parseError
        
    let private getWithQuery path query accessToken parseSuccess parseError: Async<Result<'a, 'b>> =
        sendRequest HttpMethod.Get path query None accessToken parseSuccess parseError
        
    let private post path body accessToken parseSuccess parseError: Async<Result<'a, 'b>> =
        sendRequest HttpMethod.Post path List.empty (Some body) accessToken parseSuccess parseError
        
    // Sign in
    
    let private parseSignInError (apiError: ApiError) =
        match apiError.Message with
        | "Invalid credentials." -> InvalidCredentials
        | _ -> unhandledError ()

    let private sendSignInRequest request: Async<Result<SignInResponse, SignInError>> =
        let body = JsonConvert.SerializeObject request
        post "/signIn" body None Json.deserialize<SignInResponse> parseSignInError
            
    // Sign up

    let private parseSignUpError (apiError: ApiError) =
        match apiError.Message with
        | "Account already exists." -> AccountAlreadyExists
        | "Invalid parameters." ->
            let emailError = Map.tryFind "Email" apiError.ParameterErrors
            let passwordError = Map.tryFind "Password" apiError.ParameterErrors
            InvalidSignUpParameters { EmailError = emailError; PasswordError = passwordError }
        | _ -> unhandledError ()

    let private sendSignUpRequest (request: SignUpRequest): Async<Result<SignUpResponse, SignUpError>> =
        let body = JsonConvert.SerializeObject request
        post "/signUp" body None Json.deserialize<SignUpResponse> parseSignUpError
            
    // Get shopping list

    let private sendGetShoppingListRequest accessToken: Async<GetShoppingListResponse> =
        get "/shoppingList" (Some accessToken) Json.deserialize<GetShoppingListResponse> (fun _ -> failwith "Unhandled error") |> Async.map getOk
        
         
    // Get Foodstuffs by id
        
    let private sendGetFoodstuffsByIdRequest accessToken (request: GetFoodstuffsByIdRequest): Async<GetFoodstuffsByIdResponse> =
         let query = Seq.map (fun (FoodstuffId id) -> ("ids[]", id)) request.Ids |> Seq.toList
         getWithQuery "/foodstuffs" query (Some accessToken) Json.deserialize<GetFoodstuffsByIdResponse> (fun _ -> failwith "Unhandled error.") |> Async.map getOk
         
    // Search Foodstuffs
    
    let private sendSearchFoodstuffsRequest accessToken request: Async<SearchFoodstuffsResponse> =
        if String.IsNullOrEmpty request.Term
        then
            async { return { Foodstuffs = [] } }
        else
            let query = [("query", request.Term)]
            getWithQuery "/foodstuffs/search" query (Some accessToken) Json.deserialize<SearchFoodstuffsResponse> (fun _ -> failwith "Unhandled error.") |> Async.map getOk
         
    // API Interface
    
    let unauthorized = {
        SignIn = sendSignInRequest
        SignUp = sendSignUpRequest
    }
    
    let authorized accessToken = {
        GetShoppingList = fun () -> sendGetShoppingListRequest accessToken
        GetFoodstuffsById = sendGetFoodstuffsByIdRequest accessToken
        GetRecipesById = fun _ -> failwith "Not implemented."
        SearchFoodstuffs = sendSearchFoodstuffsRequest accessToken
        AddFoodstuffsToShoppingList = fun _ -> failwith "Not implemented."
        SetFoodstuffAmountInShoppingList = fun _ -> failwith "Not implemented."
        RemoveFoodstuffs = fun _ -> failwith "Not implemented"
    }