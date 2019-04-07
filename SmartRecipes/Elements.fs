namespace SmartRecipes

module Elements =
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