module Global

type Page =
  | ExampleJSON
  | ExampleSQL
  | ExampleStringFormat

let toHash page =
  match page with
  | ExampleJSON -> "#examplejson"
  | ExampleSQL -> "#examplesql"
  | ExampleStringFormat -> "#examplestringformat"
