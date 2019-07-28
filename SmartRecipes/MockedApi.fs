namespace SmartRecipes

[<RequireQualifiedAccess>]
module MockedApi =
    open System.Net.Mail
    open System
    open Domain
    open Api
    open Domain
    open FSharpPlus.Lens
    open ShoppingList
    
    let private sampleAccessToken = {
        Value = ""
        ExpirationUtc = DateTime.UtcNow.AddDays 1.0
    }
    
    let private sampleAccount = {
        Id = AccountId "fake"
        Email = MailAddress "test@gmail.com"
    }
    
    let private sampleFoodstuffs = [
        { Id = FoodstuffId "1"; Name = "Tomato"; BaseAmount = { Value = 1.0; Unit = "pieces" }; AmountStep = 1.0 }
        { Id = FoodstuffId "2"; Name = "Onion"; BaseAmount = { Value = 1.0; Unit = "pieces" }; AmountStep = 1.0 }
        { Id = FoodstuffId "3"; Name = "Garlic"; BaseAmount = { Value = 1.0; Unit = "pieces" }; AmountStep = 1.0 }
        { Id = FoodstuffId "4"; Name = "Carrot"; BaseAmount = { Value = 1.0; Unit = "pieces" }; AmountStep = 1.0 }
    ]
    
    let private sampleFoodstuffsMap = 
        sampleFoodstuffs |> Seq.map (fun f -> (f.Id, f)) |> Map.ofSeq
        
    let private sampleRecipes = [
        {
            Id = RecipeId "1"
            Name = "Lasagne"
            CreatorId = sampleAccount.Id
            Description = "Test"
            PersonCount = 4
            ImageUrl = Uri("https://google.com")
            Ingredients = [
                { FoodstuffId = FoodstuffId "1"; Amount = Some { Value = 1.0; Unit = "pieces" } }
                { FoodstuffId = FoodstuffId "2"; Amount = Some { Value = 1.0; Unit = "pieces" } }
            ]
        };
        {
            Id = RecipeId "2"
            Name = "Burger"
            CreatorId = sampleAccount.Id
            Description = "Test"
            PersonCount = 4
            ImageUrl = Uri("https://google.com")
            Ingredients = [
                { FoodstuffId = FoodstuffId "1"; Amount = Some { Value = 1.0; Unit = "pieces" } }
                { FoodstuffId = FoodstuffId "2"; Amount = Some { Value = 1.0; Unit = "pieces" } }
            ]
        }
    ]
    
    let private sampleRecipesMap = 
        sampleRecipes |> Seq.map (fun r -> (r.Id, r)) |> Map.ofSeq
    
    let mutable private sampleShoppingList: ShoppingList = {
        Id = ShoppingListId "1"
        OwnerId = sampleAccount.Id
        Items = [
            { FoodstuffId = FoodstuffId "1"; Amount = 2.0 }
            { FoodstuffId = FoodstuffId "2"; Amount = 2.0 }
        ]
        Recipes = [
            { RecipeId = RecipeId "1"; PersonCount = 4 }
        ]
    }
    
    let recipesNotInShoppingList () =     
        List.filter (fun (r: Recipe) -> not (Seq.exists (fun i -> i.RecipeId = r.Id) sampleShoppingList.Recipes)) sampleRecipes
    
    let unauthorized = {
        SignIn = fun _ -> { SignInResponse.AccessToken = sampleAccessToken } |> Ok |> Async.id
        SignUp = fun _ -> { Account = sampleAccount } |> Ok |> Async.id
    }
    
    let authorized _ = {
        GetShoppingList = fun _ -> { GetShoppingListResponse.ShoppingList = sampleShoppingList } |> Async.id
        GetFoodstuffsById = fun r -> { GetFoodstuffsByIdResponse.Foodstuffs = List.map (fun id -> Map.find id sampleFoodstuffsMap) r.Ids } |> Async.id
        GetRecipesById = fun r -> { GetRecipesByIdResponse.Recipes = List.map (fun id -> Map.find id sampleRecipesMap) r.Ids } |> Async.id
        SearchFoodstuffs = fun r -> { Foodstuffs = List.filter (fun f -> f.Name = r.Term) sampleFoodstuffs } |> Async.id
        SearchRecipes = fun r -> { SearchRecipesResponse.Recipes = List.filter (fun f -> f.Name = r.Term) sampleRecipes } |> Async.id
        AddFoodstuffsToShoppingList = fun r ->
            let foodstuffs = List.map (fun id -> Map.find id sampleFoodstuffsMap) r.ItemIds
            let newItems = List.map (fun (f: Foodstuff) -> { FoodstuffId = f.Id; Amount = f.BaseAmount.Value }) foodstuffs
            sampleShoppingList <- over _items (fun items -> List.append newItems items) sampleShoppingList
            { AddFoodstuffsToShoppingListResponse.ShoppingList = sampleShoppingList } |> Async.id
        SetFoodstuffAmountInShoppingList = fun r ->
            sampleShoppingList <- setAmount r.FoodstuffId r.Amount sampleShoppingList
            { SetFoodstuffAmountResponse.ShoppingList = sampleShoppingList } |> Async.id
        RemoveFoodstuffs = fun r ->
            sampleShoppingList <- setl _items List.empty sampleShoppingList
            { RemoveFoodstuffsResponse.ShoppingList = sampleShoppingList } |> Async.id
        GetRecommendedRecipes = fun () -> 
            { GetRecommendedRecipesResponse.Recipes = recipesNotInShoppingList ()} |> Async.id
        AddRecipesToShoppingList = fun r -> 
            let newRecipes = List.map (fun id -> { RecipeId = id; PersonCount = 4 }) r.ItemIds
            sampleShoppingList <- over _recipeItems (List.append newRecipes) sampleShoppingList
            { AddRecipesToShoppingListResponse.ShoppingList = sampleShoppingList } |> Async.id
        RemoveRecipesFromShoppingList = fun r ->
            sampleShoppingList <- over _recipeItems (List.filter (fun i -> not (List.contains i.RecipeId r.Ids))) sampleShoppingList
            { RemoveRecipesFromShoppingListResponse.ShoppingList = sampleShoppingList } |> Async.id
    }