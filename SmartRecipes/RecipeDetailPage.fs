namespace SmartRecipes

open System
open Domain
open Elements
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
                                yield Elements.LargeLabel(
                                    text = recipe.Name
                                )
                                
                                if recipe.Tags.Length > 0 then
                                    yield View.FlexLayout(
                                        wrap = FlexWrap.Wrap,
                                        direction = FlexDirection.Row,
                                        alignItems = FlexAlignItems.Center,
                                        horizontalOptions = LayoutOptions.Center,
                                        justifyContent = FlexJustify.Center,
                                        children = List.map Elements.Tag recipe.Tags
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
                                    yield Elements.Button(
                                        text = "Add to shopping list",
                                        command = (fun () -> dispatch Add)
                                    )
                                    
                                yield Elements.LargeLabel(
                                    text = "Serves: " + recipe.PersonCount.ToString(),
                                    horizontalOptions = LayoutOptions.Start,
                                    horizontalTextAlignment = TextAlignment.Start
                                )
                                
                                yield!
                                    match recipe.CookingTime with
                                    | Some time ->
                                        let label = Elements.LargeLabel(
                                            text = "Cooking time: " + time.Text,
                                            horizontalOptions = LayoutOptions.Start,
                                            horizontalTextAlignment = TextAlignment.Start
                                        )
                                        List.singleton label
                                    | None -> []
                                    
                                yield Elements.LargeLabel(
                                    text = "Difficulty: " + Difficulty.toString recipe.Difficulty,
                                    horizontalOptions = LayoutOptions.Start,
                                    horizontalTextAlignment = TextAlignment.Start
                                )
                                
                                for ingredient in recipe.Ingredients do
                                    yield Elements.Label(
                                        text = ingredient.DisplayLine,
                                        horizontalOptions = LayoutOptions.Start,
                                        horizontalTextAlignment = TextAlignment.Start
                                    )
                            ]
                        )
                    ]
                )
            )
        )

