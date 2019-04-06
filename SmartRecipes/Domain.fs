namespace SmartRecipes

module Domain =
    open System.Net.Mail
    open System
    
    type AccountId = AccountId of string
    
    type Account = {
        Id: AccountId
        Email: MailAddress
    }
    
    type AccessToken = {
        Value: string
        ExpirationUtc: DateTime
    }                   
    
    type Unit =
        | Liter
        | Gram
        | Piece
    
    type Amount = {
        Unit: Unit
        Value: float
    }
    
    type FoodstuffId = FoodstuffId of string
    
    type Foodstuff = {
        Id: FoodstuffId
        Name: string
        AmountStep: Amount
    }
    
    type Ingredient = {
        FoodstuffId: FoodstuffId
        Amount: float
    }
    
    type RecipeId = RecipeId of string
        
    type Recipe = {
        Id: RecipeId
        Name: string
        CreatorId: AccountId
        PersonCount: int
        ImageUrl: Uri
        Description: string
        Ingredients: Ingredient seq
    }
    
    type ShoppingListItem = {
        FoodstuffId: FoodstuffId
        Amount: float
    }
    
    type ShoppingListRecipeItem = {
        RecipeId: RecipeId
        PersonCount: int
    }
    
    type ShoppingListId = ShoppingListId of string
    
    type ShoppingList = {
        Id: ShoppingListId
        OwnerId: AccountId
        Items: ShoppingListItem seq
        RecipeItems: ShoppingListRecipeItem seq
    }