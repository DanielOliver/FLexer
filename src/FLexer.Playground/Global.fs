module Global

type Page =
  | ExampleJSON
  | ExampleSQL

let toHash page =
  match page with
  | ExampleJSON -> "#examplejson"
  | ExampleSQL -> "#examplesql"
