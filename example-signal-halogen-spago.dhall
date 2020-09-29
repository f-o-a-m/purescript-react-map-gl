{ name = "example-signal-halogen"
, dependencies = (./spago.dhall).dependencies
, packages = (./spago.dhall).packages
, sources = (./spago.dhall).sources # ["example/signal-halogen/src/**/*.purs"]
}
