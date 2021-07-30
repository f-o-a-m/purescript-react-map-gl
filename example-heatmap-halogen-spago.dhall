{ name = "example-map-halogen"
, dependencies =
      (./spago.dhall).dependencies
    # [ "either"
      , "refs"
      , "unsafe-coerce"
      , "affjax"
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
, sources =
    (./spago.dhall).sources # [ "example/heatmap-halogen/src/**/*.purs" ]
}
