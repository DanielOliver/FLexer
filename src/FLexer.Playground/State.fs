module App.State

open Elmish.Browser.Navigation
open Elmish.Browser.UrlParser
open Global

let pageParser: Parser<Page->Page,Page> =
  oneOf [
    map ExampleJSON (s "examplejson")
    map ExampleSQL (s "examplesql")
  ]

let urlUpdate (result: Option<Page>) (model: Types.Model) =
  match result with
    | None ->
        Types.Model.ExampleSQL,Navigation.modifyUrl (toHash Page.ExampleSQL)
    | Some page ->
        match model.CurrentPage, page with
        | Types.Page.ExampleJSON, Page.ExampleJSON
        | Types.Page.ExampleSQL, Page.ExampleSQL -> model, []
        | _, Page.ExampleSQL ->
            Types.Model.ExampleSQL,Navigation.modifyUrl (toHash Page.ExampleSQL)
        | _, Page.ExampleJSON ->
            Types.Model.ExampleJSON,Navigation.modifyUrl (toHash Page.ExampleJSON)
