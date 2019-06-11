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
        Ids: FoodstuffId list
    }
        
    type GetFoodstuffsByIdResponse = {
        Foodstuffs: Foodstuff list
    }
    
    // Search Foodstuffs

    type SearchFoodstuffsRequest = {
        Term: string
    }
        
    type SearchFoodstuffsResponse = {
        Foodstuffs: Foodstuff list
    }
    
    // Add foodstuffs to shopping list

    type AddFoodstuffsToShoppingListRequest = {
        ItemIds: FoodstuffId list
    }
    
    type AddFoodstuffsToShoppingListResponse = {
        ShoppingList: ShoppingList
    }
    
    // Get Recipes by id
    
    type GetRecipesByIdRequest = {
        Ids: RecipeId list
    }
    
    type GetRecipesByIdResponse = {
        Recipes: Recipe list
    }
    
    // Set foodstuff amount in shopping list

    type SetFoodstuffAmountRequest = {
        FoodstuffId: FoodstuffId
        Amount: float
    }
    
    type SetFoodstuffAmountResponse = {
        ShoppingList: ShoppingList
    }
    
    // Remove foodstuffs from shooping list
    
    type RemoveFoodstuffsRequets = {
        Ids: FoodstuffId list
    }
    
    type RemoveFoodstuffsResponse = {
        ShoppingList: ShoppingList
    }
    
    // Search Recipes

    type SearchRecipesRequest = {
        Term: string
    }
        
    type SearchRecipesResponse = {
        Recipes: Recipe list
    }
    
    // Recommend recipes
    
    type GetRecommendedRecipesResponse = {
        Recommendations: Recommendation list
    }
    
    // Add recipes to shopping list
    
    type AddRecipesToShoppingListRequest = {
        ItemIds: RecipeId list
    }
    
    type AddRecipesToShoppingListResponse = {
        ShoppingList: ShoppingList
    }
    
    // Remove recipes from shopping list
    
    type RemoveRecipesFromShoppingListRequest = {
        Ids: RecipeId list
    }
    
    type RemoveRecipesFromShoppingListResponse = {
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
        SearchRecipes: SearchRecipesRequest -> Async<SearchRecipesResponse>
        AddFoodstuffsToShoppingList: AddFoodstuffsToShoppingListRequest -> Async<AddFoodstuffsToShoppingListResponse>
        SetFoodstuffAmountInShoppingList: SetFoodstuffAmountRequest -> Async<SetFoodstuffAmountResponse>
        RemoveFoodstuffs: RemoveFoodstuffsRequets -> Async<RemoveFoodstuffsResponse>
        GetRecommendedRecipes: unit -> Async<GetRecommendedRecipesResponse>
        AddRecipesToShoppingList: AddRecipesToShoppingListRequest -> Async<AddRecipesToShoppingListResponse>
        RemoveRecipesFromShoppingList: RemoveRecipesFromShoppingListRequest -> Async<RemoveRecipesFromShoppingListResponse>
    }