namespace SmartRecipes

open Api
open AppEnvironment
open Domain
open FSharpPlus.Data
open Fabulous.Core

module State =
    
    type SignInState =
        | NotRequested
        | Loading
        | Error of Api.SignInError
    
    type UnauthorizedState = {
        SignInState: SignInState
    }
    
    type UnauthorizedMessage =
        | SignIn of Credentials
        | SignInResponse of Result<Api.SignInResponse, Api.SignInError>
        
    let init = {
        SignInState = SignInState.NotRequested
    }
    
    type AuthorizedState = Unit
    
    let signIn credentials = ReaderT(fun (env: UnauthorizedEnvironment) ->
        env.Api.SignIn { Email = credentials.Email.ToString(); Password = credentials.Password }
        |> Async.map SignInResponse)
        
    let update msg state env =
        match msg with
        | SignIn credentials ->
            { state with SignInState = Loading }, Cmd.ofReader env (signIn credentials)
        | SignInResponse response ->
            match response with
            | Ok r ->
                failwith "not implemented"
            | Result.Error e ->
                { state with SignInState = SignInState.Error e }, Cmd.none
            

