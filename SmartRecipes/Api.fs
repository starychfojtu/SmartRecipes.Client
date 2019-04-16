namespace SmartRecipes

module Api =
    open Domain
    
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
        
    // Get shopping list
    
    type GetShoppingListRequest = {
        AccessToken: AccessToken
    }
    
    type GetShoppingListResponse = {
        ShoppingList: ShoppingList
    }
         
    // Get Foodstuffs by id

    type GetFoodstuffsByIdRequest = {
        Ids: FoodstuffId seq
        AccessToken: AccessToken
    }
        
    type GetFoodstuffsByIdResponse = {
        Foodstuffs: Foodstuff seq
    }
    
    // Search Foodstuffs

    type SearchFoodstuffsRequest = {
        Term: string
        AccessToken: AccessToken
    }
        
    type SearchFoodstuffsResponse = {
        Foodstuffs: Foodstuff seq
    }
    
    // Add foodstuffs to shopping list

    type AddFoodstuffsToShoppingListRequest = {
        Ids: FoodstuffId seq
        AccessToken: AccessToken
    }
    
    type AddFoodstuffsToShoppingListResponse = {
        ShoppingList: ShoppingList
    }
    
    // Get Recipes by id
    
    type GetRecipesByIdRequest = {
        Ids: RecipeId seq
        AccessToken: AccessToken
    }
    
    type GetRecipesByIdResponse = {
        Recipes: Recipe seq
    }
    
    // API Interface
    
    type SmartRecipesApi = {
        SignIn: SignInRequest -> Async<Result<SignInResponse, SignInError>>
        SignUp: SignUpRequest -> Async<Result<SignUpResponse, SignUpError>>
        GetShoppingList: GetShoppingListRequest -> Async<GetShoppingListResponse>
        GetFoodstuffsById: GetFoodstuffsByIdRequest -> Async<GetFoodstuffsByIdResponse>
        GetRecipesById: GetRecipesByIdRequest -> Async<GetRecipesByIdResponse>
        SearchFoodstuffs: SearchFoodstuffsRequest -> Async<SearchFoodstuffsResponse>
        AddFoodstuffsToShoppingList: AddFoodstuffsToShoppingListRequest -> Async<AddFoodstuffsToShoppingListResponse>
    }