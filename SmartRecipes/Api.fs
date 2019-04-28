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
    
    type GetShoppingListResponse = {
        ShoppingList: ShoppingList
    }
         
    // Get Foodstuffs by id

    type GetFoodstuffsByIdRequest = {
        Ids: FoodstuffId seq
    }
        
    type GetFoodstuffsByIdResponse = {
        Foodstuffs: Foodstuff seq
    }
    
    // Search Foodstuffs

    type SearchFoodstuffsRequest = {
        Term: string
    }
        
    type SearchFoodstuffsResponse = {
        Foodstuffs: Foodstuff seq
    }
    
    // Add foodstuffs to shopping list

    type AddFoodstuffsToShoppingListRequest = {
        Ids: FoodstuffId seq
    }
    
    type AddFoodstuffsToShoppingListResponse = {
        ShoppingList: ShoppingList
    }
    
    // Get Recipes by id
    
    type GetRecipesByIdRequest = {
        Ids: RecipeId seq
    }
    
    type GetRecipesByIdResponse = {
        Recipes: Recipe seq
    }
    
    // Set foodstuff amount in shopping list

    type SetFoodstuffAmountRequest = {
        Id: FoodstuffId
        Value: float
    }
    
    type SetFoodstuffAmountResponse = {
        ShoppingList: ShoppingList
    }
    
    // Remove foodstuffs from shooping list
    
    type RemoveFoodstuffsRequets = {
        Ids: FoodstuffId seq
    }
    
    type RemoveFoodstuffsResponse = {
        ShoppingList: ShoppingList
    }
    
    // Recommend recipes
    
    type GetRecommendedRecipesResponse = {
        Recommendations: Recommendation seq
    }
    
    // Add recipes to shopping list
    
    type AddRecipesToShoppingListRequest = {
        Ids: RecipeId seq
    }
    
    type AddRecipesToShoppingListResponse = {
        ShoppingList: ShoppingList
    }
    
    // API Interface
    
    type UnauthorizedApi = {
        SignIn: SignInRequest -> Async<Result<SignInResponse, SignInError>>
        SignUp: SignUpRequest -> Async<Result<SignUpResponse, SignUpError>>
    }
    
    type AuthorizedApi = {
        GetShoppingList: unit -> Async<GetShoppingListResponse>
        GetFoodstuffsById: GetFoodstuffsByIdRequest -> Async<GetFoodstuffsByIdResponse>
        GetRecipesById: GetRecipesByIdRequest -> Async<GetRecipesByIdResponse>
        SearchFoodstuffs: SearchFoodstuffsRequest -> Async<SearchFoodstuffsResponse>
        AddFoodstuffsToShoppingList: AddFoodstuffsToShoppingListRequest -> Async<AddFoodstuffsToShoppingListResponse>
        SetFoodstuffAmountInShoppingList: SetFoodstuffAmountRequest -> Async<SetFoodstuffAmountResponse>
        RemoveFoodstuffs: RemoveFoodstuffsRequets -> Async<RemoveFoodstuffsResponse>
        GetRecommendedRecipes: unit -> Async<GetRecommendedRecipesResponse>
        AddRecipesToShoppingList: AddRecipesToShoppingListRequest -> Async<AddRecipesToShoppingListResponse>
    }