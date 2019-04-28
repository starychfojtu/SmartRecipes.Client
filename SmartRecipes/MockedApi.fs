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
        { Id = FoodstuffId "1"; Name = "Tomato"; BaseAmount = { Value = 1.0; Unit = Piece }; AmountStep = 1.0 }
        { Id = FoodstuffId "2"; Name = "Onion"; BaseAmount = { Value = 1.0; Unit = Piece }; AmountStep = 1.0 }
        { Id = FoodstuffId "3"; Name = "Garlic"; BaseAmount = { Value = 1.0; Unit = Piece }; AmountStep = 1.0 }
        { Id = FoodstuffId "4"; Name = "Carrot"; BaseAmount = { Value = 1.0; Unit = Piece }; AmountStep = 1.0 }
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
                { FoodstuffId = FoodstuffId "1"; Amount = { Value = 1.0; Unit = Piece } }
                { FoodstuffId = FoodstuffId "2"; Amount = { Value = 1.0; Unit = Piece } }
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
                { FoodstuffId = FoodstuffId "1"; Amount = { Value = 1.0; Unit = Piece } }
                { FoodstuffId = FoodstuffId "2"; Amount = { Value = 1.0; Unit = Piece } }
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
        RecipeItems = [
            { RecipeId = RecipeId "1"; PersonCount = 4 }
        ]
    }
    
    let recipesNotInShoppingList () =     
        Seq.filter (fun (r: Recipe) -> not (Seq.exists (fun i -> i.RecipeId = r.Id) sampleShoppingList.RecipeItems)) sampleRecipes
    
    let unauthorized = {
        SignIn = fun _ -> { SignInResponse.AccessToken = sampleAccessToken } |> Ok |> Async.id
        SignUp = fun _ -> { Account = sampleAccount } |> Ok |> Async.id
    }
    
    let authorized _ = {
        GetShoppingList = fun _ -> { GetShoppingListResponse.ShoppingList = sampleShoppingList } |> Async.id
        GetFoodstuffsById = fun r -> { GetFoodstuffsByIdResponse.Foodstuffs = Seq.map (fun id -> Map.find id sampleFoodstuffsMap) r.Ids } |> Async.id
        GetRecipesById = fun r -> { Recipes = Seq.map (fun id -> Map.find id sampleRecipesMap) r.Ids } |> Async.id
        SearchFoodstuffs = fun r -> { Foodstuffs = Seq.filter (fun f -> f.Name = r.Term) sampleFoodstuffs } |> Async.id
        AddFoodstuffsToShoppingList = fun r ->
            let foodstuffs = Seq.map (fun id -> Map.find id sampleFoodstuffsMap) r.Ids
            let newItems = Seq.map (fun (f: Foodstuff) -> { FoodstuffId = f.Id; Amount = f.BaseAmount.Value }) foodstuffs
            sampleShoppingList <- over _items (fun items -> Seq.concat [newItems; items]) sampleShoppingList
            { AddFoodstuffsToShoppingListResponse.ShoppingList = sampleShoppingList } |> Async.id
        SetFoodstuffAmountInShoppingList = fun r ->
            sampleShoppingList <- setAmount r.Id r.Value sampleShoppingList
            { SetFoodstuffAmountResponse.ShoppingList = sampleShoppingList } |> Async.id
        RemoveFoodstuffs = fun r ->
            sampleShoppingList <- setl _items Seq.empty sampleShoppingList
            { RemoveFoodstuffsResponse.ShoppingList = sampleShoppingList } |> Async.id
        GetRecommendedRecipes = fun () -> 
            { Recommendations = Seq.map (fun r -> { Recipe = r; Priority = 10; }) (recipesNotInShoppingList ())} |> Async.id
        AddRecipesToShoppingList = fun r -> 
            let newRecipes = Seq.map (fun id -> { RecipeId = id; PersonCount = 4 }) r.Ids
            sampleShoppingList <- over _recipeItems (Seq.append newRecipes) sampleShoppingList
            { AddRecipesToShoppingListResponse.ShoppingList = sampleShoppingList } |> Async.id
    }