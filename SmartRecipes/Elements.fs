namespace SmartRecipes

open Fabulous.DynamicViews
open Xamarin.Forms

module Elements =
    
    let entry value error callback = seq {
        yield View.Entry(
            text = value,
            completed = (fun v -> if v <> value then callback v),
            created = (fun e -> e.Unfocused.Add(fun args -> if value <> e.Text then callback e.Text)),
            verticalOptions = LayoutOptions.FillAndExpand
        )
        
        if Option.isSome error then
            yield  View.Label(text = Option.get error)
    }
        
    let passwordEntry value error callback = entry value error callback |> Seq.map (ViewElementExtensions.isPassword true)