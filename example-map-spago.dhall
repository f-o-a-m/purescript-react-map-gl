{ name = "example-map"
, dependencies = (./spago.dhall).dependencies
, packages = (./spago.dhall).packages
, sources = (./spago.dhall).sources # ["example/map/src/**/*.purs"]
}
