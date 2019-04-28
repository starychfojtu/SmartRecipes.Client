namespace SmartRecipes

module Elements =
    open Domain
    open Fabulous.DynamicViews
    open Xamarin.Forms
    
    let entry value callback =
        View.Entry(
            text = value,
            completed = (fun v -> if v <> value then callback v),
            textChanged = (fun args -> callback args.NewTextValue),
            created = (fun e -> e.Unfocused.Add(fun args -> if value <> e.Text then callback e.Text)),
            verticalOptions = LayoutOptions.FillAndExpand
        )
    
    let validatableEntry value error callback = seq {
        yield entry value callback
        if Option.isSome error then
            yield View.Label(text = Option.get error)
    }
       
    let passwordEntry value callback =
        entry value callback |> ViewElementExtensions.isPassword true
        
    let passwordValidatableEntry value error callback =
        validatableEntry value error callback |> Seq.map (ViewElementExtensions.isPassword true)
        
    let actionButton text command = View.Button(
        text = text,
        cornerRadius = 24,
        widthRequest = 48.0,
        heightRequest = 48.0,
        verticalOptions = LayoutOptions.Center,
        command = command
    )
        
    // Recipes
    
    let recipeCard (recipe: Recipe) acionItems =
        View.Frame(
            margin = Thickness(16.0, 8.0),
            content = View.StackLayout(
                children = [
                    yield View.StackLayout(
                        orientation = StackOrientation.Horizontal,
                        heightRequest = 64.0,
                        padding = Thickness(8.0, 0.0),
                        children = [
                            yield View.Label(
                                text = recipe.Name,
                                horizontalOptions = LayoutOptions.Start,
                                verticalOptions = LayoutOptions.Center
                            )
                            yield View.StackLayout(
                                orientation = StackOrientation.Horizontal,
                                horizontalOptions = LayoutOptions.EndAndExpand,
                                children = acionItems
                            )
                        ]
                    )
                    yield View.StackLayout(
                        orientation = StackOrientation.Horizontal,
                        heightRequest = 40.0,
                        padding = Thickness(8.0, 4.0),
                        children = [
                            yield View.Label(
                                text = recipe.PersonCount.ToString (),
                                margin = Thickness(8.0, 0.0, 0.0, 0.0),
                                horizontalOptions = LayoutOptions.Start,
                                textColor = Color.Black,
                                fontSize = "Medium",
                                verticalOptions = LayoutOptions.Center
                            )
                        ]
                    )
                ]
            )
        )