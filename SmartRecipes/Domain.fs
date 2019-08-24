namespace SmartRecipes

open System
open FSharpPlus.Lens
open Lens
open FSharp.Json

module Domain =
    open System.Net.Mail
    open System
    
    type AccountId = AccountId of string
    
    type Account = {
        Id: AccountId
        [<JsonField(Transform=typeof<Json.MailAddressTransform>)>]
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
        Amount: Amount option
        DisplayLine: string
    }
    
    type RecipeId = RecipeId of string
    
    type Difficulty =
        | Easy
        | Normal
        | Hard
        
    module Difficulty =
        let toString = function
            | Easy -> "Easy"
            | Normal -> "Normal"
            | Hard -> "Hard"

        let fromString(s: string) =
            match s.ToLowerInvariant() with
            | "easy" -> Some Easy
            | "normal" -> Some Normal
            | "hard" -> Some Hard
            | _ -> None
            
        type JsonTransform() =
            interface ITypeTransform with
                member x.targetType () = (fun _ -> typeof<string>) ()
                member x.toTargetType value = (fun (v: obj) -> (toString (v:?> Difficulty)).ToLowerInvariant() :> obj) value
                member x.fromTargetType value = (fun (v: obj) -> (fromString (v :?> string) |> Option.get) :> obj) value
        
    type NutritionInfo = {
        Grams: int
        Percents: int option
    }
    
    type NutritionPerServing = {
        Calories: int option
        Fat: NutritionInfo option
        SaturatedFat: NutritionInfo option
        Sugars: NutritionInfo option
        Protein: NutritionInfo option
        Carbs: NutritionInfo option
    }
    
    type CookingTime = {
        Text: string
    }
        
    type Recipe = {
        Id: RecipeId
        Name: string
        CreatorId: AccountId
        PersonCount: int
        [<JsonField(Transform=typeof<Transforms.UriTransform>)>]
        ImageUrl: Uri
        [<JsonField(Transform=typeof<Transforms.UriTransform>)>]
        Url: Uri
        Description: string
        Ingredients: Ingredient list
        [<JsonField(Transform=typeof<Difficulty.JsonTransform>)>]
        Difficulty: Difficulty
        Rating: int option
        Tags: string list
        CookingTime: CookingTime option
        NutritionPerServing: NutritionPerServing
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