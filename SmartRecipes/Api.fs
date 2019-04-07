namespace SmartRecipes

[<RequireQualifiedAccess>]
module Api =
    open System.IO
    open System.Net
    open System.Net.Mail
    open Domain
    open FSharp.Data
    open FSharpx.Control
    open Newtonsoft.Json
    open System
    open Library

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

    type SignInRequest = {
        Email: string
        Password: string
    }
    
    type SignInResponse = {
        AccessToken: AccessToken
    }
    
    type SignInError =
        | InvalidCredentials
    
    type private SignInResponseJson = JsonProvider<""" {
        "value": "random-token-hash",
        "accountId": "guid",
        "expirationUtc": "2019-03-25T23:49:18"
    } """>
    
    let private parseSignInResponse value =
        let json = SignInResponseJson.Parse value
        { AccessToken = { Value = json.Value; ExpirationUtc = json.ExpirationUtc } }

    let private parseSignInError apiError =
        match apiError.Message with
        | "Invalid credentials." -> InvalidCredentials
        | _ -> unhandledError ()

    let sendSignInRequest email password: Async<Result<SignInResponse, SignInError>> =
        let body = JsonConvert.SerializeObject { Email = email; Password = password }
        post "/signIn" body None parseSignInResponse parseSignInError
            
    // Sign up

    type SignUpRequest = {
        Email: string
        Password: string
    }
    
    type SignUpResponse = {
        Account: Account
    }
    
    type SignUpParametersError = {
        EmailError: string option
        PasswordError: string option
    }
    
    type SignUpError =
        | AccountAlreadyExists
        | InvalidSignUpParameters of SignUpParametersError
    
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

    let sendSignUpRequest email password: Async<Result<SignUpResponse, SignUpError>> =
        let body = JsonConvert.SerializeObject { Email = email; Password = password }
        post "/signUp" body None parseSignUpResponse parseSignUpError
            
    // Get shopping list
    
    type GetShoppingListResponse = {
        ShoppingList: ShoppingList
    }
    
    type private GetShoppingListResponseJson = JsonProvider<""" {
        "id": "guid",
        "ownerId": "guid",
        "items": [{ "foodstuffId": "guid", "amount": "20.0" }],
        "recipeItems": [{ "recipeId": "guid", "personCount": "4" }]
    } """>
    
    let private parseGetShoppingListResponse value =
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

    let sendGetShoppingListRequest accessToken: Async<GetShoppingListResponse> =
        get "/shoppingList" (Some accessToken) parseGetShoppingListResponse (fun _ -> failwith "Unhandled error") |> Async.map getOk
        
         
    // Get Foodstuffs by id
    
    type GetFoodstuffsByIdRequest = {
        Ids: string seq
    }

    type GetFoodstuffByIdResponse = {
        Foodstuffs: Foodstuff seq
    }
        
    type private GetFoodstuffsByIdResponseJson = JsonProvider<""" [{
        "id": "guid",
        "name": "tomato",
        "amountStep": { "unit": "grams", "value": "20.0" }
    }] """>
    
    let private parseUnit = function
        | "grams" -> Gram
        | "pieces" -> Piece
        | "liters" -> Liter
        | _ -> failwith "Unknown unit."
    
    let private parseFoodstuff (item: GetFoodstuffsByIdResponseJson.Root) =
        {
            Id = FoodstuffId item.Id
            Name = item.Name
            AmountStep = {
                Unit = parseUnit item.AmountStep.Unit
                Value = (float)item.AmountStep.Value
            }
        }
    
    let private parseGetFoodstuffsByIdResponse value =
        let values = GetFoodstuffsByIdResponseJson.Parse value
        let foodstuffs = Array.map parseFoodstuff values
        { Foodstuffs = foodstuffs }
        
    let sendGetFoodstuffsByIdRequest accessToken ids: Async<GetFoodstuffByIdResponse> =
         let query = List.map (fun (FoodstuffId id) -> ("ids[]", id)) ids
         getWithQuery "/foodstuffs" query (Some accessToken) parseGetFoodstuffsByIdResponse (fun _ -> failwith "Unhandled error.") |> Async.map getOk
         
    // Get Recipes by id
    
    type GetRecipesByIdRequest = {
        Ids: string seq
    }
    
    type GetRecipesByIdResponse = {
        Recipes: Recipe seq
    }
    
    let sendGetRecipesByIdRequest accessToken ids: Async<GetRecipesByIdResponse> =
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
    
    
