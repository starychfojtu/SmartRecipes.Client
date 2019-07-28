namespace SmartRecipes

module RecipeDetailPage =
    open Domain
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
            content = View.StackLayout(
                children = [
                    if model.CanBeAdded then
                        yield Elements.actionButton "Add" (fun () -> dispatch Add)
                        
                    yield View.Label "Name:"
                    yield View.Label recipe.Name
                    yield View.Label "Person count:"
                    yield View.Label (recipe.PersonCount.ToString())
                    yield View.Label "Description:"
                    yield View.Editor(
                        text = recipe.Description,
                        isEnabled = false
                    )
                ]                 
            )
        )

