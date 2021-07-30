{ name = "example-map-halogen"
, dependencies =
      (./spago.dhall).dependencies
    # [ "avar"
      , "control"
      , "foldable-traversable"
      , "halogen-subscriptions"
      , "aff"
      , "aff-bus"
      , "web-html"
      , "react-dom"
      , "partial"
      , "integers"
      , "halogen"
      ]
, packages = (./spago.dhall).packages
, sources = (./spago.dhall).sources # [ "example/map-halogen/src/**/*.purs" ]
}
