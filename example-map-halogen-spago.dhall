{ name = "example-map-halogen"
, dependencies = (./spago.dhall).dependencies
, packages = (./spago.dhall).packages
, sources = (./spago.dhall).sources # ["example/map-halogen/src/**/*.purs"]
}
