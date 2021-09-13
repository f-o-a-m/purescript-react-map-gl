{ name = "example-map"
, dependencies = (./spago.dhall).dependencies # ["web-html", "react-dom", "web-dom", "partial", "integers"]
, packages = (./spago.dhall).packages
, sources = (./spago.dhall).sources # ["example/map/src/**/*.purs"]
}
