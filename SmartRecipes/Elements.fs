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
    
    let headingFontSize = 24
    
    let private entry placeholder value callback =
        View.Entry(
            text = value,
            completed = (fun v -> if v <> value then callback v),
            textChanged = (fun args -> callback args.NewTextValue),
            created = (fun e -> e.Unfocused.Add(fun args -> if value <> e.Text then callback e.Text)),
            verticalOptions = LayoutOptions.FillAndExpand,
            placeholder = placeholder,
            textColor = Colors.textDark
        )
    
    let validatableEntry placeholder value error callback = seq {
        yield entry placeholder value callback
        if Option.isSome error then
            yield View.Label(text = Option.get error, textColor = Colors.textDark)
    }
       
    let passwordEntry value callback =
        entry "Password" value callback |> ViewElementExtensions.isPassword true
        
    let passwordValidatableEntry value error callback =
        validatableEntry "Password" value error callback |> Seq.map (ViewElementExtensions.isPassword true)
        
    let actionButton text command =
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
        
    type ListRefresh =
        | No
        | Has of (unit -> unit)
        | Refreshing
        
    let private smartRecipesList (items: 'a[]) itemView onTapped refresh rowHeight =
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

    let private refreshListPage isLoading items itemView onTapped refresh emptyText rowHeight =
         let refresh = if isLoading then ListRefresh.Refreshing else ListRefresh.Has refresh
         View.Grid(
            padding = 16.0,
            rowdefs = [box "*"],
            rowSpacing = 0.0,
            children = [
                yield (smartRecipesList items itemView onTapped refresh rowHeight).GridRow(0)
                yield View.Label(
                    text = emptyText,
                    horizontalTextAlignment = TextAlignment.Center,
                    verticalOptions = LayoutOptions.Center,
                    fontSize = headingFontSize,
                    isVisible = (items.Length = 0 && not isLoading),
                    textColor = Colors.textDark
                ).GridRow(0)
            ]
        )
         
    // Recipes
    
    let tag text =
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
    
    let recipeCard acionItems (recipe: Recipe) =
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
                                verticalOptions = LayoutOptions.Center,
                                textColor = Colors.textDark
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
                                fontSize = "Medium",
                                verticalOptions = LayoutOptions.Center,
                                textColor = Colors.textDark
                            )
                        ]
                    )
                ]
            )
        )
        
    // View extension
    
    type Elements() =
        static member Entry(placeholder, value, callback) =
            entry placeholder value callback
        
        static member RefreshListPageContent(isLoading, items, itemView, onTapped, refresh, emptyText, rowHeight) =
            refreshListPage isLoading items itemView onTapped refresh emptyText rowHeight
        
        static member List(items, itemView, onTapped, refresh, rowHeight) =
            smartRecipesList items itemView onTapped refresh rowHeight