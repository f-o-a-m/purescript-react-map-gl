{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "react-map-gl"
, dependencies =
  [ "console"
  , "effect"
  , "foreign"
  , "foreign-object"
  , "functions"
  , "maybe"
  , "newtype"
  , "nullable"
  , "prelude"
  , "psci-support"
  , "react"
  , "record"
  , "simple-json"
  , "tuples"
  , "web-mercator"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
