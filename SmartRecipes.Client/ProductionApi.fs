namespace SmartRecipes

[<RequireQualifiedAccess>]
module ProductionApi =
    open System.IO
    open System.Net
    open Domain
    open FSharp.Data
    open System
    open Library
    open Api

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
        ParameterErrors: ApiParameterErrorJson list
    }
    
    let private unhandledError () =
        failwith "Unhandled API error."

    let private baseUri = 
        System.Uri("https://smart-recipes.herokuapp.com/")
    
    let private parseApiError value =
        printfn "Recieved error response %s" value
        let error = Json.deserialize<ApiErrorJson> value
        let parameterErrors = error.ParameterErrors |> Seq.map (fun e -> (e.Parameter, e.Message)) |> Map.ofSeq
        { ApiError.Message = error.Message; ParameterErrors = parameterErrors }
            
    let private sendRequest httpMethod (path: string) query body accessToken parseSuccess parseError: Async<Result<'a, 'b>> =
        async {
            try
                let authorization = Option.map (fun (t: AccessToken) -> t.Value) accessToken
                let textRequest = Option.map (TextRequest) body
                
                let queryString = String.Join(", ", List.map (fun (k, v) -> k + ":" + v) query)
                printfn "Sending request %s %s %s" path queryString (Option.defaultValue "None" body)
                
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
                    
                printfn "Recieved response %s" (response.Body.ToString())
                
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
        
    let private successPost<'req, 'res> path accessToken (request: 'req)  = 
        post
            path
            (Json.serialize request)
            (Some accessToken)
            Json.deserialize<'res>
            (fun _ -> failwith "Unhandled error.")
        |> Async.map getOk
        
    let private successGet<'res> path accessToken  = 
        get
            path
            (Some accessToken)
            Json.deserialize<'res>
            (fun _ -> failwith "Unhandled error.")
        |> Async.map getOk
        
    // Sign in
    
    let private parseSignInError (apiError: ApiError) =
        match apiError.Message with
        | "Invalid credentials." -> InvalidCredentials
        | _ -> unhandledError ()

    let private sendSignInRequest request: Async<Result<SignInResponse, SignInError>> =
        let body = Json.serialize request
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
        post "/signUp" (Json.serialize request) None Json.deserialize<SignUpResponse> parseSignUpError
            
    // Get shopping list

    let private sendGetShoppingListRequest accessToken: Async<GetShoppingListResponse> =
        get "/shoppingList" (Some accessToken) Json.deserialize<GetShoppingListResponse> (fun _ -> failwith "Unhandled error") |> Async.map getOk
        
         
    // Get Foodstuffs by id
        
    let private sendGetFoodstuffsByIdRequest accessToken (request: GetFoodstuffsByIdRequest): Async<GetFoodstuffsByIdResponse> =
         let query = Seq.map (fun (FoodstuffId id) -> ("ids[]", id)) request.Ids |> Seq.toList
         if Seq.isEmpty query
             then Async.id { Foodstuffs = [] }
             else getWithQuery "/foodstuffs" query (Some accessToken) Json.deserialize<GetFoodstuffsByIdResponse> (fun _ -> failwith "Unhandled error.") |> Async.map getOk
         
    // Get Foodstuffs by id
        
    let private sendGetRecipesByIdRequest accessToken (request: GetRecipesByIdRequest): Async<GetRecipesByIdResponse> =
         let query = Seq.map (fun (RecipeId id) -> ("ids[]", id)) request.Ids |> Seq.toList
         if Seq.isEmpty query
            then Async.id { Recipes = [] }
            else getWithQuery "/recipes" query (Some accessToken) Json.deserialize<GetRecipesByIdResponse> (fun _ -> failwith "Unhandled error.") |> Async.map getOk
         
    // Search Foodstuffs
    
    let private sendSearchFoodstuffsRequest accessToken (request: SearchFoodstuffsRequest) : Async<SearchFoodstuffsResponse> =
        if String.IsNullOrEmpty request.Term
        then
            Async.id { Foodstuffs = [] }
        else
            let query = [("query", request.Term)]
            getWithQuery "/foodstuffs/search" query (Some accessToken) Json.deserialize<SearchFoodstuffsResponse> (fun _ -> failwith "Unhandled error.") |> Async.map getOk
         
    // Search Foodstuffs
    
    let private sendSearchRecipesRequest accessToken request: Async<SearchRecipesResponse> =
        if String.IsNullOrEmpty request.Term
        then
            Async.id { Recipes = [] }
        else
            let query = [("query", request.Term)]
            getWithQuery "/recipes/search" query (Some accessToken) Json.deserialize<SearchRecipesResponse> (fun _ -> failwith "Unhandled error.") |> Async.map getOk

    
    // Add foostuff to shopping list

    let private parseAddFoodstuffToShoppingListError (apiError: ApiError) =
        match apiError.Message with
        | "Item already added." -> FoodstuffAlreadyAdded
        | _ -> unhandledError ()
    
    let private sendAddFoodstuffToShoppingList accessToken (request: AddFoodstuffsToShoppingListRequest) =
        post "/shoppingList/addFoodstuffs" (Json.serialize request) (Some accessToken) Json.deserialize<AddFoodstuffsToShoppingListResponse> parseAddFoodstuffToShoppingListError
        
    // Set foodstuff amount in shopping list

    let private parseSetFoodstuffAmountInShoppingListError (apiError: ApiError) =
        match apiError.Message with
        | "Foodstuff not in list." -> SetFoodstuffAmountError.FoodstuffNotInList
        | _ -> unhandledError ()
    
    let private sendSetFoodstuffAmountInShoppingList accessToken (request: SetFoodstuffAmountRequest) =
        post "/shoppingList/changeAmount"  (Json.serialize request) (Some accessToken) Json.deserialize<SetFoodstuffAmountResponse> parseSetFoodstuffAmountInShoppingListError
        
    // Remove foodstuff amount in shopping list

    let private parseRemoveFoodstuffsError (apiError: ApiError) =
        match apiError.Message with
        | "Foodstuff not in list." -> FoodstuffNotInList
        | _ -> unhandledError ()
    
    let private sendRemoveFoodstuffFromShoppingList accessToken (request: RemoveFoodstuffsRequets) =
        post "/shoppingList/removeFoodstuffs" (Json.serialize request) (Some accessToken) Json.deserialize<RemoveFoodstuffsResponse> parseRemoveFoodstuffsError
        
    // Remove foodstuff amount in shopping list

    let private parseAddRecipesToShoppingListError (apiError: ApiError) =
        match apiError.Message with
        | "Item already added." -> RecipeAlreadyInShoppingList
        | _ -> unhandledError ()
    
    let private sendAddRecipesToShoppingList accessToken (request: AddRecipesToShoppingListRequest) =
        post "/shoppingList/addRecipes" (Json.serialize request) (Some accessToken) Json.deserialize<AddRecipesToShoppingListResponse> parseAddRecipesToShoppingListError
        
    // Remove foodstuff amount in shopping list

    let private parseRemoveRecipesFromShoppingListError (apiError: ApiError) =
        match apiError.Message with
        | "Recipe not in list." -> RecipeNotInShoppingList
        | _ -> unhandledError ()
    
    let private sendRemoveRecipesFromShoppingList accessToken (request: RemoveRecipesFromShoppingListRequest) =
        post "/shoppingList/removeRecipes" (Json.serialize request) (Some accessToken) Json.deserialize<RemoveRecipesFromShoppingListResponse> parseRemoveRecipesFromShoppingListError
        
    // Recommend
    
    let private sendRecommend =
        successGet<GetRecommendedRecipesResponse> "/shoppingList/recommend"
         
    // API Interface
    
    let unauthorized = {
        SignIn = sendSignInRequest
        SignUp = sendSignUpRequest
    }
    
    let authorized accessToken = {
        GetShoppingList = fun () -> sendGetShoppingListRequest accessToken
        GetFoodstuffsById = sendGetFoodstuffsByIdRequest accessToken
        GetRecipesById = sendGetRecipesByIdRequest accessToken
        SearchFoodstuffs = sendSearchFoodstuffsRequest accessToken
        SearchRecipes = sendSearchRecipesRequest accessToken
        AddFoodstuffsToShoppingList = sendAddFoodstuffToShoppingList accessToken
        SetFoodstuffAmountInShoppingList = sendSetFoodstuffAmountInShoppingList accessToken
        RemoveFoodstuffs = sendRemoveFoodstuffFromShoppingList accessToken
        GetRecommendedRecipes = fun () -> sendRecommend accessToken
        AddRecipesToShoppingList = sendAddRecipesToShoppingList accessToken
        RemoveRecipesFromShoppingList = sendRemoveRecipesFromShoppingList accessToken
    }