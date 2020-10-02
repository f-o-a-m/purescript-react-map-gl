{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "react-map-gl"
, dependencies =
  [ "aff"
  , "aff-bus"
  , "console"
  , "effect"
  , "generics-rep"
  , "prelude"
  , "psci-support"
  , "react-dom"
  , "simple-json"
  , "web-mercator"
  , "debug"

  -- examples
  , "halogen"
  , "affjax"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
