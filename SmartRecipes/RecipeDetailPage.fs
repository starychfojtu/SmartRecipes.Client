namespace SmartRecipes

open Domain
open Elements
open Xamarin.Forms
open System.IO
open System.Net

module RecipeDetailPage =
    open Fabulous.DynamicViews
    
    type Model = {
        Recipe: Recipe
        Image: byte[]
    }
    
    type Message =
        | Add
        
    let initModel recipe = 
        let webClient = new WebClient()
        let image = webClient.DownloadData(recipe.ImageUrl)
        {
            Recipe = recipe
            Image = image
        }
    
    type UpdateResult = 
        | RecipeAdded
            
    let update model message =
        match message with
        | Add ->
            RecipeAdded
    
    let view dispatch model showAdd =
        let recipe = model.Recipe
        let imageStream = new MemoryStream(model.Image)
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
                        
                        yield View.Image(
                            source = ImageSource.FromStream(fun () -> imageStream :> Stream),
                            aspect = Aspect.AspectFill
                        )
                        
                        yield View.StackLayout(
                            padding = Thickness(horizontalSize = 16.0, verticalSize = 0.0),
                            children = [
                                if showAdd then
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
                                            text = "Cooking time: " + time.Text.Trim(),
                                            horizontalOptions = LayoutOptions.Start,
                                            horizontalTextAlignment = TextAlignment.Start
                                        )
                                        List.singleton label
                                    | None -> []
                                    
                                //yield!
                                //    match recipe.Difficulty with
                                //    | Some d ->
                                //        List.singleton <| Elements.LargeLabel(
                                //            text = "Difficulty: " + Difficulty.toString d,
                                //            horizontalOptions = LayoutOptions.Start,
                                //            horizontalTextAlignment = TextAlignment.Start
                                //        )
                                //    | None -> []
                                
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

