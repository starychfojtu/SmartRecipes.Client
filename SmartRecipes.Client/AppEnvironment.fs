namespace SmartRecipes

module AppEnvironment =
    open Api
    open Domain
    open FSharpPlus.Data
    
    type UnauthorizedEnvironment = {
        Api: UnauthorizedApi
    }

    type AuthorizedEnvironment = {
        Api: AuthorizedApi
    }
    
    type Environment = {
        Unauthorized: UnauthorizedEnvironment
        GetAuthorized: AccessToken -> AuthorizedEnvironment
    }

