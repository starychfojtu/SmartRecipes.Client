namespace SmartRecipes

[<RequireQualifiedAccess>]
module MockedApi =
    open System.Net.Mail
    open System
    open Domain
    open Api
    
    let private sampleAccessToken = {
        Value = ""
        ExpirationUtc = DateTime.UtcNow.AddDays 1.0
    }
    
    let private sampleAccount = {
        Id = AccountId "fake"
        Email = MailAddress "test@gmail.com"
    }
    
    let private sampleFoodstuffs = [
        { Id = FoodstuffId "1"; Name = "Tomato"; AmountStep = { Value = 1.0; Unit = Piece } }
        { Id = FoodstuffId "2"; Name = "Onion"; AmountStep = { Value = 1.0; Unit = Piece } }
        { Id = FoodstuffId "3"; Name = "Garlic"; AmountStep = { Value = 1.0; Unit = Piece } }
        { Id = FoodstuffId "4"; Name = "Carrot"; AmountStep = { Value = 1.0; Unit = Piece } }
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
                { FoodstuffId = FoodstuffId "1"; Amount = 1.0 }
                { FoodstuffId = FoodstuffId "2"; Amount = 1.0 }
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
    
    let instance = {
        SignIn = fun _ -> { SignInResponse.AccessToken = sampleAccessToken } |> Ok |> Async.id
        SignUp = fun _ -> { Account = sampleAccount } |> Ok |> Async.id
        GetShoppingList = fun _ -> { GetShoppingListResponse.ShoppingList = sampleShoppingList } |> Async.id
        GetFoodstuffsById = fun r -> { GetFoodstuffsByIdResponse.Foodstuffs = Seq.map (fun id -> Map.find id sampleFoodstuffsMap) r.Ids } |> Async.id
        GetRecipesById = fun r -> { Recipes = Seq.map (fun id -> Map.find id sampleRecipesMap) r.Ids } |> Async.id
        SearchFoodstuffs = fun r -> { Foodstuffs = Seq.filter (fun f -> f.Name = r.Term) sampleFoodstuffs } |> Async.id
        AddFoodstuffsToShoppingList = fun r ->
            let foodstuffs = Seq.map (fun id -> Map.find id sampleFoodstuffsMap) r.Ids
            let items = Seq.map (fun (f: Foodstuff) -> { FoodstuffId = f.Id; Amount = f.AmountStep.Value }) foodstuffs
            let newItems = Seq.concat [ sampleShoppingList.Items; items ]
            sampleShoppingList = { sampleShoppingList with Items = newItems } |> ignore
            { AddFoodstuffsToShoppingListResponse.ShoppingList = sampleShoppingList  }|> Async.id
    }