{ name = "example-fillextrusion-halogen"
, dependencies =
      (./spago.dhall).dependencies
    # [ "refs"
      , "unsafe-coerce"
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
      , "tailrec"
      ]
, packages = (./spago.dhall).packages
, sources =
    (./spago.dhall).sources # [ "example/fillextrusion-halogen/src/**/*.purs" ]
}
