let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.7-20230306/packages.dhall
        sha256:0757626c7422b8b5b5b1d0df3d3628e5deac755d7f89c433a9bf89009787dcbd

let overrides = {=}

let additions =
      { web-mercator =
        { dependencies = [ "partial", "prelude", "functions" ]
        , repo = "https://github.com/f-o-a-m/purescript-web-mercator.git"
        , version = "purs-0.15"
        }
      }

in  upstream // overrides // additions
