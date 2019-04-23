namespace SmartRecipes

[<RequireQualifiedAccess>]
module ProductionApi =
    open System.IO
    open System.Net
    open System.Net.Mail
    open Domain
    open FSharp.Data
    open FSharpx.Control
    open Newtonsoft.Json
    open System
    open Library
    open Api

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
    
    let private unhandledError () = failwith "Unhandled API error."

    let private baseUri = System.Uri("https://smart-recipes.herokuapp.com/")
    
    let private parseApiError value =
        let json = ApiErrorJson.Parse value
        let parameterErrors = json.ParameterErrors |> Array.map (fun (e: ApiErrorJson.ParameterError) -> (e.Parameter, e.Message)) |> Map.ofSeq
        { Message = json.Message; ParameterErrors = parameterErrors }
            
    let private sendRequest httpMethod (path: string) query body accessToken parseSuccess parseError: Async<Result<'a, 'b>> =
        async {
            try
                let authorization = Option.map (fun (t: AccessToken) -> t.Value) accessToken
                let textRequest = Option.map (TextRequest) body
                let! response =
                    match textRequest with // Why doesn't the library accept Option of body ?
                    | Some r ->
                        Http.AsyncRequest(
                            Uri(baseUri, path).ToString(),
                            query = query,
                            httpMethod = httpMethod,
                            body = r,
                            headers = seq {
                                if Option.isSome authorization then yield ("authorization", Option.get authorization)
                                yield ("Content-Type", "application/json")
                            }
                        )
                    | None ->
                        Http.AsyncRequest(
                            Uri(baseUri, path).ToString(),
                            query = query,
                            httpMethod = httpMethod,
                            headers = seq {
                                if Option.isSome authorization then yield ("authorization", Option.get authorization)
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
    
    type private SignInResponseJson = JsonProvider<""" {
        "value": "random-token-hash",
        "accountId": "guid",
        "expirationUtc": "2019-03-25T23:49:18"
    } """>
    
    let private parseSignInResponse value: SignInResponse =
        let json = SignInResponseJson.Parse value
        { AccessToken = { Value = json.Value; ExpirationUtc = json.ExpirationUtc } }

    let private parseSignInError apiError =
        match apiError.Message with
        | "Invalid credentials." -> InvalidCredentials
        | _ -> unhandledError ()

    let private sendSignInRequest request: Async<Result<SignInResponse, SignInError>> =
        let body = JsonConvert.SerializeObject request
        post "/signIn" body None parseSignInResponse parseSignInError
            
    // Sign up

    type private SignUpResponseJson = JsonProvider<""" {
        "id": "random-token-hash",
        "email": "test@gmail.com"
    } """>
    
    let private parseSignUpResponse value =
        let json = SignUpResponseJson.Parse value
        { Account = { Id = AccountId json.Id; Email = MailAddress json.Email } }
        
    let private parseSignUpError apiError =
        match apiError.Message with
        | "Account already exists." -> AccountAlreadyExists
        | "Invalid parameters." ->
            let emailError = Map.tryFind "Email" apiError.ParameterErrors
            let passwordError = Map.tryFind "Password" apiError.ParameterErrors
            InvalidSignUpParameters { EmailError = emailError; PasswordError = passwordError }
        | _ -> unhandledError ()

    let private sendSignUpRequest (request: SignUpRequest): Async<Result<SignUpResponse, SignUpError>> =
        let body = JsonConvert.SerializeObject request
        post "/signUp" body None parseSignUpResponse parseSignUpError
            
    // Get shopping list
    
    type private GetShoppingListResponseJson = JsonProvider<""" {
        "id": "guid",
        "ownerId": "guid",
        "items": [{ "foodstuffId": "guid", "amount": "20.0" }],
        "recipeItems": [{ "recipeId": "guid", "personCount": "4" }]
    } """>
    
    let private parseGetShoppingListResponse value: GetShoppingListResponse =
        let json = GetShoppingListResponseJson.Parse value
        let items = Array.map (fun (i: GetShoppingListResponseJson.Item) -> { FoodstuffId = FoodstuffId i.FoodstuffId; Amount = (float)i.Amount }) json.Items
        let recipeItems = Array.map (fun (i: GetShoppingListResponseJson.RecipeItem) -> { RecipeId = RecipeId i.RecipeId; PersonCount = i.PersonCount }) json.RecipeItems
        {
            ShoppingList = {
                Id = ShoppingListId json.Id
                OwnerId = AccountId json.OwnerId
                Items = items
                RecipeItems = recipeItems
            }
        }

    let private sendGetShoppingListRequest accessToken: Async<GetShoppingListResponse> =
        get "/shoppingList" (Some accessToken) parseGetShoppingListResponse (fun _ -> failwith "Unhandled error") |> Async.map getOk
        
         
    // Get Foodstuffs by id

    type private FoodstuffsResponseJson = JsonProvider<""" [{
        "id": "guid",
        "name": "tomato",
        "amountStep": { "unit": "grams", "value": "20.0" }
    }] """>
    
    let private parseUnit = function
        | "grams" -> Gram
        | "pieces" -> Piece
        | "liters" -> Liter
        | _ -> failwith "Unknown unit."
    
    let private parseFoodstuff (item: FoodstuffsResponseJson.Root) =
        {
            Id = FoodstuffId item.Id
            Name = item.Name
            AmountStep = {
                Unit = parseUnit item.AmountStep.Unit
                Value = (float)item.AmountStep.Value
            }
        }
    
    let private parseGetFoodstuffsByIdResponse value: GetFoodstuffsByIdResponse =
        let values = FoodstuffsResponseJson.Parse value
        let foodstuffs = Array.map parseFoodstuff values
        { Foodstuffs = foodstuffs }
        
    let private sendGetFoodstuffsByIdRequest accessToken (request: GetFoodstuffsByIdRequest): Async<GetFoodstuffsByIdResponse> =
         let query = Seq.map (fun (FoodstuffId id) -> ("ids[]", id)) request.Ids |> Seq.toList
         getWithQuery "/foodstuffs" query (Some accessToken) parseGetFoodstuffsByIdResponse (fun _ -> failwith "Unhandled error.") |> Async.map getOk
         
    // Search Foodstuffs
    
    let private parseSearchFoodstuffsResponse value: SearchFoodstuffsResponse =
        let values = FoodstuffsResponseJson.Parse value
        let foodstuffs = Array.map parseFoodstuff values
        { Foodstuffs = foodstuffs }
    
    let private sendSearchFoodstuffsRequest accessToken request: Async<SearchFoodstuffsResponse> =
        if String.IsNullOrEmpty request.Term
        then
            async { return { Foodstuffs = [] } } // TODO: fix this in API
        else
            let query = [("query", request.Term)]
            getWithQuery "/foodstuffs/search" query (Some accessToken) parseSearchFoodstuffsResponse (fun _ -> failwith "Unhandled error.") |> Async.map getOk

         
    // Get Recipes by id

    let private sendGetRecipesByIdRequest accessToken (request: GetRecipesByIdRequest): Async<GetRecipesByIdResponse> =
         async {
             return {
                 Recipes = [
                     {
                         Id = RecipeId "r1"
                         Name = "Lasagna"
                         CreatorId = AccountId "guid"
                         PersonCount = 4
                         ImageUrl = Uri("https://google.com")
                         Description = "Very good"
                         Ingredients = [
                             { FoodstuffId = FoodstuffId "f1"; Amount = 10.0 }
                         ]
                     }
                 ]
             }
        }
         
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
        AddFoodstuffsToShoppingList = fun _ -> failwith "Not implemented."
        SetFoodstuffAmountInShoppingList = fun _ -> failwith "Not implemented."
    }