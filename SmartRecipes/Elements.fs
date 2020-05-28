namespace SmartRecipes

open Xamarin.Forms

module Colors =
    let primary = Color.FromHex("#1976d2") 
    let primaryDark = Color.FromHex("#004ba0")
    let primaryLight = Color.FromHex("#63a4ff")
    let textDark = Color.FromRgba(0, 0, 0, 222)
    let textLight = Color.FromRgb(255, 255, 255)

module Elements =
    open Domain
    open Fabulous.DynamicViews
    
    let fontSize = 16
    let headingFontSize = 24
    
    type ListRefresh =
        | No
        | Has of (unit -> unit)
        | Refreshing
        
    type Elements() =
           
        static member Label(text, ?isVisible, ?horizontalOptions, ?verticalOptions, ?horizontalTextAlignment) =
            View.Label(
                text = text,
                horizontalTextAlignment = defaultArg horizontalTextAlignment TextAlignment.Center,
                verticalTextAlignment = TextAlignment.Center,
                ?horizontalOptions = horizontalOptions,
                ?verticalOptions = verticalOptions,
                fontSize = fontSize,
                ?isVisible = isVisible,
                textColor = Colors.textDark
            )
            
        static member LargeLabel(text, ?isVisible, ?horizontalOptions, ?verticalOptions, ?horizontalTextAlignment) =
            View.Label(
                text = text,
                horizontalTextAlignment = defaultArg horizontalTextAlignment TextAlignment.Center,
                verticalTextAlignment = TextAlignment.Center,
                ?horizontalOptions = horizontalOptions,
                ?verticalOptions = verticalOptions,
                fontSize = headingFontSize,
                ?isVisible = isVisible,
                textColor = Colors.textDark
            )
            
        static member Entry(placeholder, value, callback) =
            View.Entry(
                text = value,
                completed = (fun v -> if v <> value then callback v),
                textChanged = (fun args -> callback args.NewTextValue),
                created = (fun e -> e.Unfocused.Add(fun args -> if value <> e.Text then callback e.Text)),
                verticalOptions = LayoutOptions.FillAndExpand,
                placeholder = placeholder,
                textColor = Colors.textDark
            )
            
        static member ValidatableEntry(placeholder, value, error, callback) =
            seq {
                yield Elements.Entry(placeholder, value, callback)
                yield!
                    match error with
                    | Some e -> [Elements.LargeLabel(text = e)]
                    | None -> []
            }

        static member PasswordEntry(value, callback) =
            Elements.Entry(placeholder = "Password", value = value, callback = callback)
            |> ViewElementExtensions.isPassword true

        static member PasswordValidatableEntry(value, error, callback) =
            Elements.ValidatableEntry(placeholder = "Password", value = value, error = error, callback = callback)
            |> Seq.map (ViewElementExtensions.isPassword true)
            
        static member Button(text, command) =
            View.Button(
                text = text,
                verticalOptions = LayoutOptions.FillAndExpand,
                command = command,
                textColor = Colors.textLight,
                backgroundColor = Colors.primaryDark
            )
            
        static member RoundedButton(text, command) =
            View.Button(
                text = text,
                cornerRadius = 24,
                widthRequest = 48.0,
                heightRequest = 48.0,
                verticalOptions = LayoutOptions.Center,
                command = command,
                textColor = Colors.textLight,
                backgroundColor = Colors.primaryDark
            )

        static member List(items: 'a[], itemView, onTapped, refresh, rowHeight) =
            let (isRefreshEnabled, isRefreshing) =
                match refresh with
                | No -> false, false
                | Has _ -> true, false
                | Refreshing _ -> true, true
                
            View.ListView(
                rowHeight = rowHeight,
                items = Seq.map itemView items,
                selectionMode = ListViewSelectionMode.None,
                itemTapped = (fun i -> onTapped items.[i]),
                separatorVisibility = SeparatorVisibility.None,
                isPullToRefreshEnabled = isRefreshEnabled,
                refreshCommand = (fun () ->
                    match refresh with
                    | No -> ()
                    | Has f -> f ()
                    | Refreshing -> ()
                ),
                isRefreshing = isRefreshing
            )

        static member Tag(text) =
            View.ContentView(
                padding = Thickness(horizontalSize = 2.0, verticalSize = 0.0),
                backgroundColor = Colors.primaryLight,
                margin = Thickness(4.0),
                content = View.Label(
                    text = text,
                    lineBreakMode = LineBreakMode.NoWrap,
                    textColor = Colors.textLight,
                    heightRequest = 24.0,
                    horizontalTextAlignment = TextAlignment.Center,
                    verticalTextAlignment = TextAlignment.Center
                )
            )
            
        static member FoodstuffCard(actions, foodstuff: Foodstuff, amount: float) =
            View.StackLayout(
                orientation = StackOrientation.Horizontal, 
                children = [
                    yield View.StackLayout(
                        verticalOptions = LayoutOptions.CenterAndExpand,
                        children = [
                            yield View.Label(
                                text = foodstuff.Name,
                                horizontalOptions = LayoutOptions.Start,
                                verticalOptions = LayoutOptions.Center
                            )
                            yield View.Label(
                                text = amount.ToString() + " " + foodstuff.BaseAmount.Unit.ToString(),
                                verticalOptions = LayoutOptions.Center
                            )
                        ]                 
                    )
                    yield View.StackLayout(
                        horizontalOptions = LayoutOptions.EndAndExpand,
                        orientation = StackOrientation.Horizontal,
                        children = actions              
                    )
                ]         
            )

        static member RecipeCard(actionItems, recipe, foodstuffInShoppingList) =
            let ingredientsInShoppingList = 
                recipe.Ingredients
                |> List.filter (fun i -> Set.contains i.FoodstuffId foodstuffInShoppingList)

            let recipeName = 
                if recipe.Name.Length > 30
                    then recipe.Name.Substring(0, 30) + "..."
                    else recipe.Name

            View.Frame(
                margin = Thickness(16.0, 8.0),
                content = View.StackLayout(
                    children = [
                        yield View.StackLayout(
                            orientation = StackOrientation.Horizontal,
                            heightRequest = 64.0,
                            padding = Thickness(8.0, 0.0),
                            children = [
                                yield Elements.Label(
                                    text = recipeName,
                                    horizontalOptions = LayoutOptions.Start,
                                    verticalOptions = LayoutOptions.Center
                                )
                                yield View.StackLayout(
                                    orientation = StackOrientation.Horizontal,
                                    horizontalOptions = LayoutOptions.EndAndExpand,
                                    children = actionItems
                                )
                            ]
                        )
                        yield View.StackLayout(
                            orientation = StackOrientation.Horizontal,
                            heightRequest = 40.0,
                            padding = Thickness(8.0, 4.0),
                            children = [
                                yield Elements.Label(
                                    text = ingredientsInShoppingList.Length.ToString() + " " + (if ingredientsInShoppingList.Length = 1 then "ingredient" else "ingredients") + " in your list",
                                    horizontalOptions = LayoutOptions.Start,
                                    verticalOptions = LayoutOptions.Center
                                )
                            ]
                        )
                    ]
                )
            )
            
        static member RefreshListPageContent(isLoading, items, itemView, onTapped, refresh, emptyText, rowHeight) =
            let refresh = if isLoading then ListRefresh.Refreshing else ListRefresh.Has refresh
            View.Grid(
                padding = 16.0,
                rowdefs = [box "*"],
                rowSpacing = 0.0,
                children = [
                    yield Elements.List(items, itemView, onTapped, refresh, rowHeight).GridRow(0)
                    yield Elements.LargeLabel(
                        text = emptyText,
                        isVisible = (items.Length = 0 && not isLoading)
                    ).GridRow(0)
                ]
            )