{ name = "his-name-o!"
, dependencies =
  [ "aff"
  , "aff-promise"
  , "arrays"
  , "console"
  , "effect"
  , "enums"
  , "foldable-traversable"
  , "halogen"
  , "integers"
  , "lists"
  , "maybe"
  , "ordered-collections"
  , "prelude"
  , "strings"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
