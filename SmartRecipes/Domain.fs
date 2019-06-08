namespace SmartRecipes

open FSharpPlus.Lens
open Lens

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
    
    type Amount = {
        Unit: string
        Value: float
    }
    
    type FoodstuffId = FoodstuffId of string
    
    type Foodstuff = {
        Id: FoodstuffId
        Name: string
        AmountStep: float
        BaseAmount: Amount
    }
    
    type Ingredient = {
        FoodstuffId: FoodstuffId
        Amount: Amount
    }
    
    type RecipeId = RecipeId of string
        
    type Recipe = {
        Id: RecipeId
        Name: string
        CreatorId: AccountId
        PersonCount: int
        ImageUrl: Uri
        Description: string
        Ingredients: Ingredient list
    }
    
    type ShoppingListItem = {
        FoodstuffId: FoodstuffId
        Amount: float
    }
    
    module ShoppingListItem =
        let inline _amount f item = f item.Amount <&> fun v -> { item with Amount = v }
    
    type ShoppingListRecipeItem = {
        RecipeId: RecipeId
        PersonCount: int
    }
    
    type ShoppingListId = ShoppingListId of string
    
    type ShoppingList = {
        Id: ShoppingListId
        OwnerId: AccountId
        Items: ShoppingListItem list
        Recipes: ShoppingListRecipeItem list
    }

    module ShoppingList =
        open ShoppingListItem
        
        let inline _items f shoppingList = f shoppingList.Items <&> fun v -> { shoppingList with Items = v }
        let inline _recipeItems f shoppingList = f shoppingList.Recipes <&> fun v -> { shoppingList with Recipes = v }
        
        let setAmount foodstuffId value shoppingList: ShoppingList =
            setl (_items << (_where (fun i -> i.FoodstuffId = foodstuffId)) << _amount) value shoppingList
            
    type Recommendation = {
        Recipe: Recipe
        Priority: int
    }