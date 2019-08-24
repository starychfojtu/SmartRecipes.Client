namespace SmartRecipes

open System
open Domain
open Xamarin.Forms

module RecipeDetailPage =
    open Fabulous.DynamicViews
    
    type Model = {
        Recipe: Recipe
        CanBeAdded: bool
    }
    
    type Message =
        | Add
        
    let initModel recipe canBeAdded = {
        Recipe = recipe
        CanBeAdded = canBeAdded
    }
    
    type UpdateResult = 
        | RecipeAdded
            
    let update model message =
        match message with
        | Add ->
            RecipeAdded
    
    let view dispatch model =
        let recipe = model.Recipe
        View.ContentPage(
            content = View.ScrollView(
                content = View.StackLayout(
                    padding = Thickness(horizontalSize = 0.0, verticalSize = 16.0),
                    children = [
                        yield View.StackLayout(
                            padding = Thickness(horizontalSize = 16.0, verticalSize = 0.0),
                            children = [
                                yield View.Label(
                                    text = recipe.Name,
                                    horizontalTextAlignment = TextAlignment.Center,
                                    fontSize = Elements.headingFontSize,
                                    margin = Thickness(horizontalSize = 16.0, verticalSize = 0.0)
                                )
                                
                                if recipe.Tags.Length > 0 then
                                    yield View.FlexLayout(
                                        wrap = FlexWrap.Wrap,
                                        direction = FlexDirection.Row,
                                        alignItems = FlexAlignItems.Center,
                                        horizontalOptions = LayoutOptions.Center,
                                        justifyContent = FlexJustify.Center,
                                        children = List.map Elements.tag recipe.Tags
                                    )
                            ]
                        )
                        
                        // TODO: recipe url.
                        yield View.Image(
                            source = ImageSource.FromUri(Uri("https://www.thewholesomedish.com/wp-content/uploads/2018/07/Best-Lasagna-550.jpg")),
                            aspect = Aspect.AspectFill
                        )
                        
                        yield View.StackLayout(
                            padding = Thickness(horizontalSize = 16.0, verticalSize = 0.0),
                            children = [
                                if model.CanBeAdded then
                                    yield View.Button(
                                        text = "Add to shopping list",
                                        horizontalOptions = LayoutOptions.CenterAndExpand,
                                        command = (fun () -> dispatch Add)
                                    )
                                    
                                yield View.Label(
                                    text = "Serves " + recipe.PersonCount.ToString(),
                                    fontSize = Elements.headingFontSize
                                )
                                
                                yield!
                                    match recipe.CookingTime with
                                    | Some time ->
                                        let label =  View.Label(
                                            text = "Cooking time: " + time.Text,
                                            fontSize = Elements.headingFontSize
                                        )
                                        List.singleton label
                                    | None -> []
                                    
                                yield View.Label(
                                    text = "Difficulty: " + Difficulty.toString recipe.Difficulty,
                                    fontSize = Elements.headingFontSize
                                )
                                
                                for ingredient in recipe.Ingredients do
                                    yield View.Label(
                                        text = ingredient.DisplayLine
                                    )
                            ]
                        )
                    ]
                )
            )
        )

