module App.State

open Elmish.Browser.Navigation
open Elmish.Browser.UrlParser
open Global

let pageParser: Parser<Page->Page,Page> =
  oneOf [
    map ExampleJSON (s "examplejson")
    map ExampleSQL (s "examplesql")
  ]

let urlUpdate (result: Option<Page>) model =
  match result with
    | None ->
        Types.Model.ExampleSQL,Navigation.modifyUrl (toHash Page.ExampleSQL)
    | Some page ->
        model, []
